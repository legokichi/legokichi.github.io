## 動機

現状のブラウザ環境では、
pngからピクセル情報を得るためには
一旦canvasに書き込んでおいてからgetImageDataする必要がある。

このときpngをデコードするためにUIスレッドでdeflateが呼ばれるため、
大量の画像ファイルのピクセル情報にアクセスしようとすると非常に時間がかかる。

WebWorkerスレッドで並列的にpngをデコードしてピクセル処理したいが、
WebWorkerではcanvasが使えない(CanvasProxyというのが策定中のようだ？)([OffscreenCanvas](https://developer.mozilla.org/ja/docs/Web/API/OffscreenCanvas)というのもあるらしい)


そこで、WebWorker上で[png.js](https://github.com/devongovett/png.js/)などのJSライブラリでデコードする必要があるのだが、ネイティブのデコードに比べれば遅い。
wasmが実用化すればlibpngをwasm化してデコードできるようになるので、
今回はとりあえずlibpngをemscriptenしてみた。


## 作業環境

[emsdk](https://kripken.github.io/emscripten-site/docs/tools_reference/emsdk.html)で作った

```
$ emcc --version
emcc (Emscripten gcc/clang-like replacement) 1.35.0 ()
Copyright (C) 2014 the Emscripten authors (see AUTHORS.txt)
This is free and open source software under the MIT license.
There is NO warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

$ clang --version
clang version 3.7.0 (https://github.com/kripken/emscripten-fastcomp-clang dbe68fecd03d6f646bd075963c3cc0e7130e5767) (https://github.com/kripken/emscripten-fastcomp 4e83be90903250ec5142edc57971ed4c633c5e25)
Target: x86_64-apple-darwin15.4.0
Thread model: posix

$ where clang
/Users/***/emsdk_portable/clang/e1.35.0_64bit/clang
/usr/bin/clang

$ where emcc
/Users/***/emsdk_portable/emscripten/1.35.0/emcc
```

## zlib と libpng のダウンロード

libpngはzlibを使用しているので両方のソースをダウンロードする

```sh
wget http://zlib.net/zlib-1.2.8.tar.gz
wget http://download.sourceforge.net/libpng/libpng-1.6.21.tar.gz
tar zxvf zlib-1.2.8.tar.gz
tar zxvf libpng-1.6.21.tar.gz
rm zlib-1.2.8.tar.gz libpng-1.6.21.tar.gz
```

### zlib のllvm bitcode化

コンパイル&リンクして ひとつのllvm bitcodeファイルにする

```sh
cd zlib-1.2.8
emconfigure ./configure
sed -i -e "s/^AR=.*$/AR=emcc/" Makefile
sed -i -e "s/libz\.a/libz.bc/" Makefile
emmake make
```

emcc は `.a` 拡張子でllvm bcを出力するのを許さないので `.bc` に変更する

`libtool` ではllvm bcをリンクできないのでemccに書き換える

これで `libz.bc` ができた


### libpng のllvm bitcode化

```sh
cd libpng-1.6.21
sed -i -e "s/\#define PNG_ZLIB_VERNUM 0x1250/#define PNG_ZLIB_VERNUM 0x1280/g" ./pnglibconf.h
sed -i -e "s/ZPREFIX\=\'z\_\'/ZPREFIX='..\/zlib-1.2.8\/'/g" ./configure
emconfigure ./configure --with-zlib-prefix='../zlib-1.2.8/'
sed -i -e "s/^DEFAULT_INCLUDES \= \-I\./DEFAULT_INCLUDES = -I. -I..\/zlib-1.2.8\//g" ./Makefile
sed -i -e "s/^LIBS \= \-lz/LIBS = -L..\/zlib-1.2.8\//g" ./Makefile
emmake make --include-dir=../zlib-1.2.8/
emcc -static  -fno-common -DPIC  .libs/png.o .libs/pngerror.o .libs/pngget.o .libs/pngmem.o .libs/pngpread.o .libs/pngread.o .libs/pngrio.o .libs/pngrtran.o .libs/pngrutil.o .libs/pngset.o .libs/pngtrans.o .libs/pngwio.o .libs/pngwrite.o .libs/pngwtran.o .libs/pngwutil.o   -L../zlib-1.2.8/ -lc    -Wl,-soname -Wl,libpng16.16.dylib -Wl,-retain-symbols-file -Wl,libpng.sym -o .libs/libpng16.16.bc
```

zlib の `zlib.h` の `ZLIB_VERNUM` と
libpng の `pnglibconf.h` の `PNG_ZLIB_VERNUM` が一致していなければ無理やり一致させる(API変わっていたらダメなので本来は一致するzlib | libpngのバージョンを用意する)

また、configure の `--with-zlib-prefix` オプションが emconfigure を使うと何故か効かなくなるので `ZPREFIX` へ直接 zlib へのパスを書き込む

libpngの Makefile では共有ライブラリしか作れず、emccは共有ライブラリに対応していない（[将来的にできるかもねとのこと](https://github.com/kripken/emscripten/wiki/Linking)）ので、改めてスタティックライブラリを作る


これで `.libs/libpng16.16.bc` ができる

### 自分のコードに組み込む

```sh
emcc -std=c11 -Wall -I./zlib-1.2.8 -I./libpng-1.6.21 -o ./obj/main.o -c ./src/main.c
emcc -O1 -o ./bin/a.out.js --pre-js ./src/em-pre.js --post-js ./src/em-post.js ./zlib-1.2.8/libz.bc ./libpng-1.6.21/.libs/libpng16.16.bc ./obj/main.o
```

コンパイルは普通に zlib と libpng のヘッダファイルのディレクトリを `-I` で参照させる

リンクでは `./zlib-1.2.8/libz.bc` と `./libpng-1.6.21/.libs/libpng16.16.bc` を含めること

## JSから呼ぶ

emscripten の [ccall](https://kripken.github.io/emscripten-site/docs/api_reference/preamble.js.html#ccall) などを利用し、JS側から呼ぶ。

libpngのAPIはファイルポインタを必要とするため、
[FileSystem API](https://kripken.github.io/emscripten-site/docs/api_reference/Filesystem-API.html) を用いて事前にpngファイルのバッファを仮想FSに書き込んでおく必要がある。

## サンプル

https://github.com/legokichi/libpng-emscripten-sandbox


## 参考

* http://dencha.ojaru.jp/programs_07/pg_graphic_10a2.html
* http://invar6.blog.fc2.com/category3-1.html
* http://invar6.blog.fc2.com/blog-entry-9.html
* http://diary.jdigital.be/toshi/mingw_gui/013.html
* http://gmoon.jp/png/
* https://kripken.github.io/emscripten-site/docs/compiling/Building-Projects.html
* http://d.hatena.ne.jp/sleepy_yoshi/20090510/p1
* http://www.ysr.net.it-chiba.ac.jp/data/cc.html
* https://github.com/kripken/emscripten/wiki/WebAssembly
* https://github.com/kripken/emscripten/wiki/Linking
* https://kripken.github.io/emscripten-site/docs/api_reference/preamble.js.html#ccall
