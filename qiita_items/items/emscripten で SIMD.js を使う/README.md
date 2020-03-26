# emscripten で SIMD.js を使う

[コンパイル時とリンク時に `emcc -s SIMD` する](https://kripken.github.io/emscripten-site/docs/porting/simd.html)。

SIMD.js が使われるケースは3種類ある。

1. clang デフォルトの [LLVM autovectorization](http://llvm.org/docs/Vectorizers.html) が働いて自動的に SIMD 化する
2. GCC 拡張の [SIMD Vector Extensions](https://gcc.gnu.org/onlinedocs/gcc/Vector-Extensions.html) で明示的に使う
3. x86 の [Streaming SIMD Extensions](https://ja.wikipedia.org/wiki/Streaming_SIMD_Extensions) 命令を使う。詳細は [大本営](https://kripken.github.io/emscripten-site/docs/porting/simd.html) を読むべし

opencv など sse サポートしているものをを asm.js にする場合は 3 が使えそう

## SIMD.js が有効なブラウザを用意する

`-s SIMD` で JS 出力すると SIMD.js の polyfill が付いてくる。
これをそのまま使うと遅くなる。

[2017年上半期現在標準で SIMD が有効なブラウザはない](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/SIMD)。

chrome の場合起動フラグでV8の起動フラグを渡せば有効になる。 

`chrome --js-flags="--harmony-simd"`

使える V8 起動フラグの一覧を見るには `chrome --js-flags="--help"`

フラグ指定して起動する方法は[リンク参照](https://www.chromium.org/developers/how-tos/run-chromium-with-flags)。


## ビルド例(opencv の場合)

### 1. configure

`-s SIMD=1` をつけて cmake する

```
git clone https://github.com/opencv/opencv.git
cd opencv
mkdir build
cd build
OPTS='-O2 --llvm-lto 1 -s SIMD=1'
emcmake cmake \
  -DCMAKE_BUILD_TYPE=RELEASE \
  -DBUILD_DOCS=OFF \
  -DBUILD_EXAMPLES=OFF \
  -DBUILD_PACKAGE=OFF \
  -DBUILD_WITH_DEBUG_INFO=OFF \
  -DBUILD_opencv_cuda=OFF \
  -DBUILD_opencv_cudaarithm=OFF \
  -DBUILD_opencv_cudabgsegm=OFF \
  -DBUILD_opencv_cudacodec=OFF \
  -DBUILD_opencv_cudafeatures2d=OFF \
  -DBUILD_opencv_cudafilters=OFF \
  -DBUILD_opencv_cudaimgproc=OFF \
  -DBUILD_opencv_cudaoptflow=OFF \
  -DBUILD_opencv_cudastereo=OFF \
  -DBUILD_opencv_cudawarping=OFF \
  -DENABLE_PRECOMPILED_HEADERS=OFF \
  -DWITH_1394=OFF \
  -DWITH_CUDA=OFF \
  -DWITH_CUFFT=OFF \
  -DWITH_EIGEN=OFF \
  -DWITH_FFMPEG=OFF \
  -DWITH_GIGEAPI=OFF \
  -DWITH_GSTREAMER=OFF \
  -DWITH_GTK=OFF \
  -DWITH_JASPER=OFF \
  -DWITH_JPEG=OFF \
  -DWITH_OPENCL=OFF \
  -DWITH_OPENCLAMDBLAS=OFF \
  -DWITH_OPENCLAMDFFT=OFF \
  -DWITH_OPENEXR=OFF \
  -DWITH_PNG=OFF \
  -DWITH_PVAPI=OFF \
  -DWITH_TIFF=OFF \
  -DWITH_LIBV4L=OFF \
  -DWITH_WEBP=OFF \
  -DWITH_PTHREADS_PF=OFF \
  -DBUILD_opencv_apps=OFF \
  -DBUILD_PERF_TESTS=OFF \
  -DBUILD_TESTS=OFF \
  -DBUILD_SHARED_LIBS=OFF \
  -DENABLE_SSE=OFF \
  -DENABLE_SSE2=OFF \
  -DENABLE_SSE3=ON \
  -DENABLE_SSE41=OFF \
  -DENABLE_SSE42=OFF \
  -DENABLE_AVX=OFF \
  -DENABLE_AVX2=OFF \
  -DCMAKE_CXX_FLAGS=$OPTS \
  -DCMAKE_EXE_LINKER_FLAGS=$OPTS \
  -DCMAKE_CXX_FLAGS_DEBUG=$OPTS \
  -DCMAKE_CXX_FLAGS_RELWITHDEBINFO=$OPTS \
  -DCMAKE_C_FLAGS_RELWITHDEBINFO=$OPTS \
  -DCMAKE_EXE_LINKER_FLAGS_RELWITHDEBINFO=$OPTS \
  -DCMAKE_MODULE_LINKER_FLAGS_RELEASE=$OPTS \
  -DCMAKE_MODULE_LINKER_FLAGS_DEBUG=$OPTS \
  -DCMAKE_MODULE_LINKER_FLAGS_RELWITHDEBINFO=$OPTS \
  -DCMAKE_SHARED_LINKER_FLAGS_RELEASE=$OPTS \
  -DCMAKE_SHARED_LINKER_FLAGS_RELWITHDEBINFO=$OPTS \
  -DCMAKE_SHARED_LINKER_FLAGS_DEBUG=$OPTS \
  ../
```

`-DENABLE_SSE3=ON` にする

### 2. make

```
emmake make -j 4
```

これで　llvm bitcode な `libopencv_*.a` ができる

### 3. リンク

あらかじめ用意しておいた opencv を使用するプログラム `my_program.bc` とリンクする

```
emcc \
  ./my_program.bc \
  ./lib/libopencv_video.a \
  ./lib/libopencv_imgproc.a \
  ./lib/libopencv_core.a \
  ./3rdparty/lib/libzlib.a \
  -O2 --llvm-lto 1 -s SIMD=1 \
  -o ./my_program.bc
```

`llvm-nm my_program.bc` でシンボルが見える

基礎的な注意点として 依存するライブラリは順番を後ろにすること。
例えば libopencv_imgproc.a は libzlib.a に依存し、
libopencv_video.a は libopencv_core.a に依存している。

### 4. asm.js へコンパイル

```
emcc \
  my_program.bc \
  -O2 --llvm-lto 3 -s SIMD=1 \
  -s TOTAL_MEMORY=33554432 \
  -s ASSERTIONS=0 \
  -o ./cv.js
```

これで SIMD が有効なブラウザだと例えば `cv::calcOpticalFlowFarneback` が 4 倍ほど速くなる

