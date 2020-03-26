# Theano の OpenCL バックエンド を Core i5 Haswell の Ubuntu14.04 上で実行する

先行例: http://onlybrain.exblog.jp/25776518/

※ この記事では `pyenv shell anaconda3-4.3.0` を使用しています。

## Beignet をインストールする

haswell 世代では Intel 公式の OpenCL GPU のサポートがないので Beignet を使います。Beignet でも Haswell 世代では Linux Kernel 4.2 以下ではカーネルにパッチを当てる必要があります。`uname -r` でカーネルバージョンを確認しておきましょう。

* intel opencl の対応状況はここ - https://software.intel.com/en-us/intel-opencl
* その他のハードウェア環境 + Linux 上での GPGPU の手段については ArchLinux Wiki が明るいです - https://wiki.archlinuxjp.org/index.php/GPGPU

インストールには

```sh
sudo apt-get install beignet-opencl-icd
```

するか、もしくは https://www.freedesktop.org/wiki/Software/Beignet/ を参考にソースからビルドします。

```sh
sudo apt-get install cmake pkg-config python ocl-icd-dev libegl1-mesa-dev ocl-icd-opencl-dev libdrm-dev libxfixes-dev libxext-dev llvm-3.6-dev clang-3.6 libclang-3.6-dev libtinfo-dev libedit-dev zlib1g-dev
git clone https://anongit.freedesktop.org/git/beignet.git
cd beignet
mkdir build
cd build
cmake .. -DCMAKE_BUILD_TYPE=Release
make
sudo make install
```

ユニットテストしたい場合は

```sh
make utest
cd utests
source setenv.sh
./utest_run
```

とすればできます。

### clinfo コマンドで OpenCL 環境を確認する

```sh
sudo apt-get install clinfo
```
```sh
$ clinfo
Number of platforms                               1
  Platform Name                                   Intel Gen OCL Driver
  Platform Vendor                                 Intel
  Platform Version                                OpenCL 1.2 beignet 1.4 (git-beaf26f)
  Platform Profile                                FULL_PROFILE
  Platform Extensions                             cl_khr_global_int32_base_atomics cl_khr_global_int32_extended_atomics cl_khr_local_int32_base_atomics cl_khr_local_int32_extended_atomics cl_khr_byte_addressable_store cl_khr_3d_image_writes cl_khr_image2d_from_buffer cl_khr_depth_images cl_khr_spir cl_khr_icd cl_intel_accelerator cl_intel_subgroups cl_intel_subgroups_short cl_intel_media_block_io cl_intel_planar_yuv
  Platform Extensions function suffix             Intel
Beignet: self-test failed: (3, 7, 5) + (5, 7, 3) returned (3, 7, 5)
See README.md or http://www.freedesktop.org/wiki/Software/Beignet/
Beignet: Warning - overriding self-test failure

  Platform Name                                   Intel Gen OCL Driver
Number of devices                                 1
Beignet: Warning - overriding self-test failure
  Device Name                                     Intel(R) HD Graphics Haswell Ultrabook GT2 Mobile
  Device Vendor                                   Intel
  Device Vendor ID                                0x8086
  Device Version                                  OpenCL 1.2 beignet 1.4 (git-beaf26f)
  Driver Version                                  1.4
  Device OpenCL C Version                         OpenCL C 1.2 beignet 1.4 (git-beaf26f)
  Device Type                                     GPU
  Device Profile                                  FULL_PROFILE
  Max compute units                               20
  Max clock frequency                             1000MHz
  Device Partition                                (core)
    Max number of sub-devices                     1
    Supported partition types                     None, None, None
  Max work item dimensions                        3
  Max work item sizes                             512x512x512
  Max work group size                             512
  Preferred work group size multiple              16
  Preferred / native vector sizes                 
    char                                                16 / 8       
    short                                                8 / 8       
    int                                                  4 / 4       
    long                                                 2 / 2       
    half                                                 0 / 8        (n/a)
    float                                                4 / 4       
    double                                               0 / 2        (n/a)
  Half-precision Floating-point support           (n/a)
  Single-precision Floating-point support         (core)
    Denormals                                     No
    Infinity and NANs                             Yes
    Round to nearest                              Yes
    Round to zero                                 No
    Round to infinity                             No
    IEEE754-2008 fused multiply-add               No
    Support is emulated in software               No
    Correctly-rounded divide and sqrt operations  No
  Double-precision Floating-point support         (n/a)
  Address bits                                    32, Little-Endian
  Global memory size                              2021654528 (1.883GiB)
  Error Correction support                        No
  Max memory allocation                           1516240896 (1.412GiB)
  Unified memory for Host and Device              Yes
  Minimum alignment for any data type             128 bytes
  Alignment of base address                       1024 bits (128 bytes)
  Global Memory cache type                        Read/Write
  Global Memory cache size                        8192
  Global Memory cache line                        64 bytes
  Image support                                   Yes
    Max number of samplers per kernel             16
    Max size for 1D images from buffer            65536 pixels
    Max 1D or 2D image array size                 2048 images
    Base address alignment for 2D image buffers   4096 bytes
    Pitch alignment for 2D image buffers          1 bytes
    Max 2D image size                             8192x8192 pixels
    Max 3D image size                             8192x8192x2048 pixels
    Max number of read image args                 128
    Max number of write image args                8
  Local memory type                               Local
  Local memory size                               65536 (64KiB)
  Max constant buffer size                        134217728 (128MiB)
  Max number of constant args                     8
  Max size of kernel argument                     1024
  Queue properties                                
    Out-of-order execution                        No
    Profiling                                     Yes
  Prefer user sync for interop                    Yes
  Profiling timer resolution                      80ns
  Execution capabilities                          
    Run OpenCL kernels                            Yes
    Run native kernels                            Yes
    SPIR versions                                 1.2
  printf() buffer size                            1048576 (1024KiB)
  Built-in kernels                                __cl_copy_region_align4;__cl_copy_region_align16;__cl_cpy_region_unalign_same_offset;__cl_copy_region_unalign_dst_offset;__cl_copy_region_unalign_src_offset;__cl_copy_buffer_rect;__cl_copy_image_1d_to_1d;__cl_copy_image_2d_to_2d;__cl_copy_image_3d_to_2d;__cl_copy_image_2d_to_3d;__cl_copy_image_3d_to_3d;__cl_copy_image_2d_to_buffer;__cl_copy_image_3d_to_buffer;__cl_copy_buffer_to_image_2d;__cl_copy_buffer_to_image_3d;__cl_fill_region_unalign;__cl_fill_region_align2;__cl_fill_region_align4;__cl_fill_region_align8_2;__cl_fill_region_align8_4;__cl_fill_region_align8_8;__cl_fill_region_align8_16;__cl_fill_region_align128;__cl_fill_image_1d;__cl_fill_image_1d_array;__cl_fill_image_2d;__cl_fill_image_2d_array;__cl_fill_image_3d;
  Device Available                                Yes
  Compiler Available                              Yes
  Linker Available                                Yes
  Device Extensions                               cl_khr_global_int32_base_atomics cl_khr_global_int32_extended_atomics cl_khr_local_int32_base_atomics cl_khr_local_int32_extended_atomics cl_khr_byte_addressable_store cl_khr_3d_image_writes cl_khr_image2d_from_buffer cl_khr_depth_images cl_khr_spir cl_khr_icd cl_intel_accelerator cl_intel_subgroups cl_intel_subgroups_short cl_intel_media_block_io cl_intel_planar_yuv

NULL platform behavior
  clGetPlatformInfo(NULL, CL_PLATFORM_NAME, ...)  No platform
  clGetDeviceIDs(NULL, CL_DEVICE_TYPE_ALL, ...)   No platform
  clCreateContext(NULL, ...) [default]            No platform
  clCreateContext(NULL, ...) [other]              Success [Intel]
  clCreateContextFromType(NULL, CL_DEVICE_TYPE_CPU)  No platform
  clCreateContextFromType(NULL, CL_DEVICE_TYPE_GPU)  No platform
  clCreateContextFromType(NULL, CL_DEVICE_TYPE_ACCELERATOR)  No platform
  clCreateContextFromType(NULL, CL_DEVICE_TYPE_CUSTOM)  No platform
  clCreateContextFromType(NULL, CL_DEVICE_TYPE_ALL)  No platform
```

`source setenv.sh` をして環境変数が適切に設定されていればこのようにズラズラと情報が表示されます。



## clBLAS か CLBlast をインストールする。

Theano のバックエンドである libgpuarray を OpenCL 環境で動かすためには clBLAS　か CLBlast をインストールする必要があります。 clBlas と CLBlast の違いですが、 CLBlast のほうが特定の環境では高速なようです。両方入れた場合　CLBlast のほうが優先される？ようです。

* https://github.com/clMathLibraries/clBLAS
* https://github.com/CNugteren/CLBlast

### clBLAS　の場合

```sh
git clone https://github.com/clMathLibraries/clBLAS.git
cd clBLAS
mkdir build
cd build
cmake .. -DCMAKE_BUILD_TYPE=Release
make
sudo make install
```

### CLBlast の場合

```sh
git clone https://github.com/CNugteren/CLBlast.git
cd CLBlast
mkdir build
cd build
cmake .. -DCMAKE_BUILD_TYPE=Release
make
sudo make install
```

## libgpuarray と pygpu をインストールする

Theano のバックエンドである libgpuarray および pygpu をインストールします。conda や pip からではなくソースからのビルドが必要です。

```sh
git clone https://github.com/Theano/libgpuarray.git
cd libgpuarray
mkdir build
cd build
cmake .. -DCMAKE_BUILD_TYPE=Release
make
sudo make install
cd ..
python setup.py build
python setup.py install
```

### pygpu をテストする

pygpu が opencl を認識できているかどうかを調べます。

```sh
python -c "import pygpu; print(pygpu.init('opencl0:0').devname)"
python -c "import pygpu; print(pygpu.init('opencl0:1').devname)"
python -c "import pygpu; print(pygpu.init('opencl0:2').devname)"
```

`ValueError: No device 1` と表示されなければ OK です。
また、 `Begnet: self-test failed` と表示されてエラーが出た場合は、 

```sh
export OCL_IGNORE_SELF_TEST=1
```

とすれば少々不安ですが、beignetの実行前テストを無視して実行できるようになります。自分の環境下では `Intel(R) HD Graphics Haswell Ultrabook GT2 Mobile` と表示されます。

pygpu のセルフテストをします。

```sh
DEVICE="opencl0:0" python -c "import pygpu;pygpu.test()"
```

`.` が沢山表示されれば成功ですが　`E` もたくさん表示されるかもしれません。

`LoadError: ***.so.*: cannot open shared object file:  No such file or directory` とされた場合は共有ライブラリの情報が更新されていないかもしれないので 

```sh
sudo ldconfig
```

として依存関係情報を更新してください。

## Theano をインストールする

pygpu が手動インストールできていれば theano は pip で十分です

```sh
pip install theano
```

### Theano をテストする

http://deeplearning.net/software/theano/tutorial/using_gpu.html にある次のスクリプトを実行することで theano が GPU で動いているかどうかを確認することができます。

```python:theano_gpu_check.py
from theano import function, config, shared, tensor
import numpy
import time

vlen = 10 * 30 * 768  # 10 x #cores x # threads per core
iters = 1000

rng = numpy.random.RandomState(22)
x = shared(numpy.asarray(rng.rand(vlen), config.floatX))
f = function([], tensor.exp(x))
print(f.maker.fgraph.toposort())
t0 = time.time()
for i in range(iters):
    r = f()
t1 = time.time()
print("Looping %d times took %f seconds" % (iters, t1 - t0))
print("Result is %s" % (r,))
if numpy.any([isinstance(x.op, tensor.Elemwise) and
              ('Gpu' not in type(x.op).__name__)
              for x in f.maker.fgraph.toposort()]):
    print('Used the cpu')
else:
    print('Used the gpu')
```

フラグをつけて実行します。まず CPU で試してみます。

```sh
$ THEANO_FLAGS=mode=FAST_RUN,device=cpu,floatX=float32 python ./theano_gpu_check.py
[Elemwise{exp,no_inplace}(<TensorType(float32, vector)>)]
Looping 1000 times took 34.545774 seconds
Result is [ 1.23178029  1.61879337  1.52278066 ...,  2.20771813  2.29967761
  1.62323284]
Used the cpu
```

次に opencl で試してみます。

```sh
$ THEANO_FLAGS=mode=FAST_RUN,device=opencl0:0,floatX=float32 python ./theano_gpu_check.py
Beignet: self-test failed: (3, 7, 5) + (5, 7, 3) returned (3, 7, 5)
See README.md or http://www.freedesktop.org/wiki/Software/Beignet/
Beignet: Warning - overriding self-test failure
Beignet: Warning - overriding self-test failure
Mapped name None to device opencl0:0: Intel(R) HD Graphics Haswell Ultrabook GT2 Mobile 
[GpuElemwise{exp,no_inplace}(<GpuArrayType<None>(float32, (False,))>), HostFromGpu(gpuarray)(GpuElemwise{exp,no_inplace}.0)]
Looping 1000 times took 1.114583 seconds
Result is [ 1.23178029  1.61879337  1.52278054 ...,  2.20771813  2.29967737
  1.62323284]
Used the gpu
```

34 秒かかっていた処理が 1 秒まで短縮されました。

しかしよく見るとベクトルの足し算のテストがおかしな結果になっていたり、CPUと結果が異なっていたりと怪しい挙動をしています。

また Theano や libgpuarray の issue では[こと](https://github.com/Theano/libgpuarray/issues/402#issuecomment-293387472)ある[ごと](https://github.com/Theano/Theano/issues/5418#issuecomment-273523987)に 「OpenCL サポートは実験的でありメンテする時間がない」と述べられています。






