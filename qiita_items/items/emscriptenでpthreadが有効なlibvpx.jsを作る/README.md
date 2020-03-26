# emscriptenでpthreadが有効なlibvpx.jsを作る

[前回](http://qiita.com/DUxCA/items/c5009a17684b0e78464b)はemscriptenでlibvpxをasm.jsにしました。今回はpthreadに対応します。

## configure へのパッチ

```diff:configure.patch
diff --git a/build/make/configure.sh b/build/make/configure.sh
index ac60f50..72f6db9 100644
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
@@ -1337,6 +1337,21 @@ EOF
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
diff --git a/vpx/src/vpx_codec.c b/vpx/src/vpx_codec.c
index 10331aa..15b81c1 100644
--- a/vpx/src/vpx_codec.c
+++ b/vpx/src/vpx_codec.c
@@ -124,6 +124,7 @@ void vpx_internal_error(struct vpx_internal_error_info *info,
 
     info->has_detail = 1;
     va_start(ap, fmt);
+    printf("vpx_internal_error: %s\n", fmt);
     vsnprintf(info->detail, sz - 1, fmt, ap);
     va_end(ap);
     info->detail[sz - 1] = '\0';
```

## llvm bitcode へのビルド

```bash:build.sh
emconfigure ./configure \
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
  --target=asmjs-unknown-emscripten \
  --extra-cflags="-O2"
emmake make
```

ここまでは前回とほとんど同じで `libvpx.a` なる llvm bitcode が生成されます。
違いは `--disable-multithread` していないだけです。

## JSとのインターフェースを書く

```c:decode.c
#include "../libvpx/vpx/vpx_decoder.h"
#include "../libvpx/vpx/internal/vpx_codec_internal.h"
#include "../libvpx/vp8/decoder/onyxd_int.h"
#include "../libvpx/vp8/vp8_dx_iface.c"

#include <stdio.h>

vpx_codec_ctx_t codec;
vpx_codec_dec_cfg_t cfg;

void die_codec(vpx_codec_ctx_t *ctx, const char *s) {
  const char *detail = vpx_codec_error_detail(ctx);
  printf("%s: %s\n", s, vpx_codec_error(ctx));
  if (detail) printf("    %s\n", detail);
  exit(1);
}

int init(int argc, char **argv) {
  printf("Using %s\n", vpx_codec_iface_name(vpx_codec_vp8_dx()));
  cfg.threads = 4;
  if (vpx_codec_dec_init(&codec, vpx_codec_vp8_dx(), &cfg, 0)){
    die_codec(&codec, "Failed to initialize decoder.");
  }
  return 0;
}

int decode(const unsigned char *frame, size_t frame_size){
  printf("frame_size: %d \n",(unsigned int)frame_size);
  if (vpx_codec_decode(&codec, frame, (unsigned int)frame_size, NULL, 0)){
    die_codec(&codec, "Failed to decode frame.");
  }
  return 0;
}

int destroy(void){
  if (vpx_codec_destroy(&codec)){
    die_codec(&codec, "Failed to destroy codec");
  }
  return 0;
}

#define CM ((vpx_codec_alg_priv_t *)codec.priv)->yv12_frame_buffers.pbi[0]->common
int show_frame(){ return CM.show_frame; }
int width(){ return CM.Width; }
int height(){ return CM.Height; }
int mb_rows(){ return CM.mb_rows; }
int mb_cols(){ return CM.mb_cols; }
int mode_info_stride(){ return CM.mode_info_stride; }
#define MI(NAME, OP) \
  NAME (int row, int col){ \
    const int idx = row * CM.mode_info_stride + col; \
    MODE_INFO *mi = &CM.mi[idx]; \
    return (OP); \
  }

int MI(mbmi_ref_frame, mi->mbmi.ref_frame)
int MI(mbmi_mode, mi->mbmi.mode)
```

`init` でデコーダを初期化し、 `decode` でVP8 bitstream の毎フレームをデコードし、
各種アクセサ関数を定義してフレーム情報の入った構造体へアクセスします。
今回はフレームごとの各マクロブロックの情報を取得します。
`cfg.threads = 4;` でデコーダが使うスレッドを4つに指定しています。

## pthreadを有効にした asm.jsへのビルド

JSからアクセスする関数をJSONで書いておきます。

```json:exported_functions.json
[
  "_init",
  "_decode",
  "_destroy",
  "_show_frame",
  "_width",
  "_height",
  "_mb_rows",
  "_mb_cols",
  "_mode_info_stride",
  "_mbmi_ref_frame",
  "_mbmi_mode"
]
```

emcc で asm.js　を出力します。

```bash:build.sh
exported_functions=$(node --eval "console.log(JSON.stringify(require('./exported_functions.json')).split('\"').join('\''))
emcc \
  -s USE_PTHREADS=1 -s PTHREAD_POOL_SIZE=4 \
  -s TOTAL_MEMORY=67108864 \
  -s SIMD=1 \
  -O2 --llvm-lto 3 \
  -s EXPORTED_FUNCTIONS="$exported_functions" \
  -I. \
  ./decode.c libvpx.a \
  -o ./decode.js
```

[pthread](https://kripken.github.io/emscripten-site/docs/porting/pthreads.html) を使うには `-s USE_PTHREADS=1` します。
`-s PTHREAD_POOL_SIZE=4` でスレッドの最大数も指定できます。

ビデオのデコードは解像度に比例してヒープを消費するので `-s TOTAL_MEMORY` に大きめのヒープサイズを指定しています。

さらなる最適化を期待して llvm bitcode オブジェクトファイル結合後に最適化をかけ、その上で linktime optimization をする `--llvm-lto 3` を指定します。

これで `decode.js` `decode.mem` が生成されます。

## JSから呼び出す

MediaRecorder の作る WebM をデコードします。

```js:index.html
<script src="decode.js"></script>
<script src="EBML.js"></script>
<script src="EBMLReader.js"></script>
<script>
//window.onload = (()=>{ // .mem がないとき
Module.addOnPostRun(()=>{ // .mem があるとき
  // デコーダの準備
  const decoder = new EBML.Decoder(); // webm コンテナデコーダ
  const reader = new EBMLReader();
  Module.ccall("init", "number", ["number", "*"], []); // vpx デコーダ
  navigator.mediaDevices.getUserMedia({
    audio: true,
    video: true
  }).then((stream)=>{
    const rec = new MediaRecorder(stream, { mimeType: 'video/webm; codecs="vp8, opus"' });
    let task = Promise.resolve();
    rec.ondataavailable = (ev)=>{
      task.then(()=> readAsArrayBuffer(ev.data).then((buffer)=>{
        decoder.decode(buffer).forEach((elm)=>{
          reader.read(elm);
        });
      }) );
    };
    reader.addListener("simpleblock_video", ({elm, data})=>{
      // video の入った Simple Block のとき

      // SimpleBlock には複数の frame が入っている場合がある
      data.frames.forEach((frame)=>{
        // malloc してvp8 bitstream を asm.js のヒープへ書き込む
        const size = frame.length;
        const buf = Module._malloc(size);
        Module.HEAP8.set(frame, buf);

        // 配列ポインタ渡しでデコード
        console.time("decode");
        Module.ccall("decode", "number", ["*", "number"], [buf, size]);
        console.timeEnd("decode");

        // メモリ解放
        Module._free(buf);

        // フレーム情報を得る
        const width = Module.ccall("width", "number", [], []);
        const height = Module.ccall("height", "number", [], []);
        const rows = Module.ccall("mb_rows", "number", [], []);
        const cols = Module.ccall("mb_cols", "number", [], []);

        for (let row = 0; row < rows; ++row) {
          for (let col = 0; col < cols; ++col) {
            // マクロブロックごとの予測モード
            const mode = Module.ccall("_mbmi_mode", "number", ["number", "number"], [row, col]);
          }
        }
      });
    });

    rec.start(100);

    setTimeout(()=>{
      // 録画停止
      rec.stop();
      rec.ondataavailable = undefined;
      rec.stream.getTracks().map((track) => { track.stop(); });
    }, 10 * 1000);

  });
});

function readAsArrayBuffer(blob) {
  return new Promise((resolve, reject)=>{
    const reader = new FileReader();
    reader.readAsArrayBuffer(blob);
    reader.onloadend = ()=>{ resolve(reader.result); };
    reader.onerror = (ev)=>{ reject(ev.error); };
  });
}
</script>
```

EBML.js, EBMLReader.js は 拙作WebMコンテナパーサhttps://github.com/legokichi/ts-ebml/ です。

注意点として `*.mem` があるときと `--memory-init-file 0` でメモリファイルないときでJS側のエントリポイントが異なります。
メモリファイルがある場合、asm.jsは非同期に実行されるため、`Module.addOnPostRun(()=>{});` でイベントを登録します。
メモリファイルがない場合は `<script />` が上から順番に同期的に実行されるため、 とりあえずbodyが読み終わる `window.onload` をエントリポイントにしています。


## ブラウザで動かす

Chrome でも [SharedArrayBuffer](https://www.chromestatus.com/feature/4570991992766464) を使えるようになりました。`chrome:flags#shared-array-buffer` 拡張を有効にしてください。

現在の emscripten で生成されるpthreadを使用したコードをchromeで動かすと`Uncaught DOMException: Failed to execute 'postMessage' on 'Worker': A SharedArrayBuffer could not be cloned.` エラーが発生します。

chrome では shared array buffer を postMessage で webworker に渡すときに 暗黙に　transferable に指定されないバグ？のためです。

decode.js の `worker.postMessage({cmd:"load",..., buffer:HEAPU8.buffer,...});` を `worker.postMessage({cmd:"load",buffer:HEAPU8.buffer,...}, [HEAPU8.buffer]);` として SharedArrayBuffer である HEAP のバッファを transferable 指定すれば動きます。 

これで高解像度の動画も安心してデコードできますね！


## 作業レポジトリ

* https://github.com/legokichi/libvpx.js



