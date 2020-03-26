# emscriptenでlibvpx.jsを作る

## ビルドの流れ

`.o` `.a` アセンブリを作る代わりに `--emit-llvm` で llvm bitcode を出力し、それを emcc で asm.js にビルドする

## configure を書き換える

参考

* https://bitbucket.org/desmaj/libvpx.js/src/1ea3218282b6eb129061341831d23409dd539054/configure.patch

https://chromium.googlesource.com/webm/libvpx/+/904b957ae965bd3d67f15a75cd9db7954f810d33 時点の場合だと


```diff:configure.patch
diff --git a/build/make/configure.sh b/build/make/configure.sh
index ac60f50..f95dbdc 100644
--- a/build/make/configure.sh
+++ b/build/make/configure.sh
@@ -456,7 +456,7 @@ NM=${NM}
 
 CFLAGS  = ${CFLAGS}
 CXXFLAGS  = ${CXXFLAGS}
-ARFLAGS = -crs\$(if \$(quiet),,v)
+ARFLAGS = crs\$(if \$(quiet),,v)
 LDFLAGS = ${LDFLAGS}
 ASFLAGS = ${ASFLAGS}
 extralibs = ${extralibs}
@@ -1337,6 +1337,22 @@ EOF
           ;;
       esac
       ;;
+    asmjs-unknown-emscripten)
+      # echo "@ " $toolchain " = asmjs-unknown-emscripten""
+      # echo "@ " $tgt_isa "-" $tgt_os "-" $tgt_cc
+      CC=emcc
+      LD=llvm-link
+      AR=llvm-ar
+      AS=llvm-as
+      NM=llvm-nm
+      tune_cflags=""
+      tune_asflags=""
+      add_cflags -emit-llvm
+      #add_ldflags
+      #add_asflags 
+      disabled multithread
+      HAVE_GNU_STRIP=no
+      ;;
     *-gcc|generic-gnu)
       link_with_cc=gcc
       enable_feature gcc
diff --git a/configure b/configure
index 379c2f4..deb0965 100755
--- a/configure
+++ b/configure
@@ -155,6 +155,7 @@ all_platforms="${all_platforms} x86_64-win64-vs11"
 all_platforms="${all_platforms} x86_64-win64-vs12"
 all_platforms="${all_platforms} x86_64-win64-vs14"
 all_platforms="${all_platforms} generic-gnu"
+all_platforms="${all_platforms} asmjs-unknown-emscripten"
 
 # all_targets is a list of all targets that can be configured
 # note that these should be in dependency order for now.
```

のようにすればよい

* `llvm-ar` は `ar` と違ってオプションに `-` がつかない

## ビルドの手順

参考

* https://bitbucket.org/desmaj/libvpx.js/src/1ea3218282b6eb129061341831d23409dd539054/build.sh
* https://github.com/bemasc/Broadway/blob/master/vp8/README


```bash:build.sh
git clone https://chromium.googlesource.com/webm/libvpx
cd libvpx
patch -p1 -i ./configure.patch
source /path/to/emsdk_env.sh
emsdk activate latest
emconfigure ./configure \
  --disable-optimizations \
  --disable-runtime-cpu-detect \
  --disable-examples \
  --disable-docs \
  --disable-vp9 \
  --disable-vp8-encoder \
  --disable-unit_tests \
  --disable-install_bins \
  --disable-install_libs \
  --disable-encode_perf_tests \
  --disable-decode_perf_tests \
  --disable-vp8_encoder \
  --target=asmjs-unknown-emscripten \
  --extra-cflags="-O2"
emmake make
exported_functions="['_vpx_codec_version_str', '_vpx_codec_dec_init_ver', '_vpx_codec_enc_init_ver', '_vpx_codec_vp8_dx', '_vpx_codec_iface_name', '_vpx_codec_err_to_string', '_vpx_codec_error_detail', '_vpx_codec_error', '_vpx_codec_decode', '_vpx_codec_get_frame', '_vpx_codec_encode', '_vpx_codec_get_cx_data', '_vpx_img_alloc']"
emcc \
  -s ALLOW_MEMORY_GROWTH=1 \
  -s EXPORTED_FUNCTIONS="$exported_functions" \
  libvpx.a \
  -o libvpx.js
```

* vp8 の decoder だけほしかったので `--disable-vp9` `--disable-vp8-encoder` をした。
* `-s ALLOW_MEMORY_GROWTH=1` の代わりに `-O2` ができたら asm.js　の最適化が働いて嬉しい
* 

## JS との Interface
* JS との interface の例は下記が参考になる。 必要に応じてこのような JS を書いて `--post-js` するとよい
  * https://bitbucket.org/desmaj/libvpx.js/src/1ea3218282b6eb129061341831d23409dd539054/epilogue.libvpx.js
  * https://bitbucket.org/desmaj/libvpx.js/src/1ea3218282b6eb129061341831d23409dd539054/examples/browser/encode.js

## libvpx の使い方について

* libvpx そのものの使い方は libvpx のリポジトリのドキュメントやサンプルコードを追えばよい
  * https://chromium.googlesource.com/webm/libvpx/+/904b957ae965bd3d67f15a75cd9db7954f810d33/examples/simple_decoder.c
  * https://chromium.googlesource.com/webm/libvpx/+/904b957ae965bd3d67f15a75cd9db7954f810d33/usage.dox
  * https://chromium.googlesource.com/webm/libvpx/+/904b957ae965bd3d67f15a75cd9db7954f810d33/usage_dx.dox


## VP8 、 VP9 の 詳細について

### VP8

* 圧縮方式、実際のバイナリの格納方式 - https://tools.ietf.org/html/rfc6386
* 圧縮の概要 - http://blog.webmproject.org/2010/07/inside-webm-technology-vp8-intra-and.html

### VP9

* 圧縮方式、実際のバイナリの格納方式 - https://storage.googleapis.com/downloads.webmproject.org/docs/vp9/vp9-bitstream-specification-v0.6-20160331-draft.pdf
* バイナリの格納方式 - http://downloads.webmproject.org/docs/vp9/vp9-bitstream_superframe-and-uncompressed-header_v1.0.pdf
* 概要 - https://blogs.gnome.org/rbultje/2016/12/13/overview-of-the-vp9-video-codec/
* 概要 - https://forum.doom9.org/showthread.php?t=168947


### emscripten の情報を得るには

* https://github.com/kripken/emscripten を clone して git grep すると学びがある
  * https://github.com/kripken/emscripten/tree/master/site/source/docs にはまだ書かれていないことも多い
* https://github.com/kripken/emscripten/issues で検索する
