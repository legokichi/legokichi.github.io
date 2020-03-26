
* __※ nvidia-docker2 が発表されたため以下の知識はすべて過去のものとなりました。[公式wiki](https://github.com/NVIDIA/nvidia-docker/wiki)が充実しているのでそちらをみたほうが良いです__
* __※ この記事は、この記事の古いバージョンを改定編集し 2017年10月21日 の [技術書典3](https://techbookfest.org/event/tbf03) にて頒布した同人誌のさらに加筆編集版です__

# nvidia-docker でポータブルな機械学習環境を作る

## 動機

caffe, caffe2, tensorflow, theano, mxnet, chainer, pytorch, torch などの様々な CUDA 依存のライブラリやフレームワークがある。
しかしこれらは互いに依存するubuntuやpython、CUDA等のバージョンがそれぞれ異なる。
このような状況では以下のような問題が発生する。

* 共用 GPU マシンの場合 - ユーザがそれぞれ sudo 権限を持ち、好き勝手にライブラリをインストールしたりバージョンアップすると、すぐに環境が破綻する
* 研究での GPU 利用の場合 - その時その時のマシンの環境依存になってしまうため、再現性が担保できない
* クラウドサービス利用の場合 - 仮想マシンから GPU を利用するのが（不可能ではないが）面倒くさい、計算に使いたいのに仮想化によるオーバーヘッドがある

これらの問題は nvidia-docker を使うことで解決できる。

* 共用 GPU マシンの場合 - ユーザはそれぞれ自分の docker コンテナ内で作業すればよい
* 研究での GPU 利用の場合 - Dockerfile に再現手順を記述できるため、再現性が高い
* クラウドサービス利用の場合 - GPUデバイスに直接アクセスできる、コンテナ仮想化なのでオーバーヘッドが少ない


### docker とは

VMWare や VirtualBox に代表されるハードウェア仮想化は基本的にはハードウェアエミュレータである。
一方 Docker は Linux Container (LXC) という Linux カーネルに組み込まれたコンテナ仮想化技術を使っている。
コンテナ仮想化は、あるプロセスがカーネルを通じてアクセスできる環境を分割し、仮想化している。
通常カーネルはプロセスに対して仮想メモリアドレスを与えているが、その延長として、LXCはプロセスID、ファイルシステム、IPC、ネットワーク、CPU、デバイスなどの名前空間をプロセスごおとに分割して提供することで、プロセスをとりまく環境を仮想化している。
Docker はこの LXC を利用しつつ、これに加えてファイルシステムの差分管理、仮想ブリッジなどを提供している。

以下参考リンク

* Docker を支える Linux Kernel の機能 (概要編) - http://blog.etsukata.com/2014/05/docker-linux-kernel.html
* Docker内部で利用されているLinuxカーネルの機能 (namespace/cgroups) - https://qiita.com/wellflat/items/7d62f2a63e9fcddb31cc
* Linuxカーネル Docker関連 namespaceのメモ - http://rest-term.com/archives/3287/

以後 __ホスト__ とはdockerデーモンが動いているLinuxマシンのことを指して使う。

### nvidia-docker とは

Docker でそのまま NVIDIA GPU を扱おうとすると、コンテナ内に NVIDIA ドライバを完全に再インストールしてから、ホストにあるデバイスファイル（`/dev/nvidia0` など）をコンテナの起動時に渡す必要があった。
しかしこの方法ではホストのドライバとコンテナのドライバのバージョンが完全に一致していなければ動かない。一致していないと例えばこののようなエラーが出る。

```sh
$ nvidia-smi 
Failed to initialize NVML: Driver/library version mismatch
```

これではコンテナに合わせてホストドライバをインストールする必要があるわけで、コンテナ仮想化の意義が失われてしまう。
そこで nvidia-docker はコンテナ起動時にホストの NVIDIA ドライバとデバイスファイルを Docker volume として渡してやることで、コンテナ側のドライバインストールを不要としている。

nvidia-docker は docker コマンドのラッパである nvidia-docker コマンドと、
ホストマシンの GPU 環境を調べ、コンテナ起動時に必要なデバイスファイルとドライバをが入った docker volume を作成する nvidia-docker-plugin デーモンから成る。
この nvidia-docker-plugin デーモンが nvidia-docker 技術のキモである。

NVIDIA ドライバはいくつかのカーネルモジュール（例： `nvidia`）と、それを利用するための共有ライブラリ（例： `libcuda.so` 、 `libnvcuvid.so`）から成る。
しかしどのディレクトリにどのバージョンのライブラリがあって、どれをコンテナに渡すのが適切なのかの判断は、GPU、ホストOS、ドライバのバージョンなどによって異なり、難しい。
そこで、この nvidia-docker-plugin デーモンがコンテナに渡すべきファイルをリストアップし、 docker volume としてひとつのディレクトリにまとめてくれる。
docker volume とはコンテナ内からホストマシンのファイルへアクセスするための技術で、 nvidia-docker-plugin の場合は `/var/lib/nvidia-docker/volumes/nvidia_driver/361.48` あたりにドライバファイルをまとめたディレクトリを用意し、コンテナ内の `/usr/local/nvidia` にマウントする。

```console
$ docker volume list
DRIVER              VOLUME NAME
nvidia-docker       nvidia_driver_375.66
$ docker volume inspect nvidia_driver_375.66
[
    {
        "CreatedAt": "0001-01-01T00:00:00Z",
        "Driver": "nvidia-docker",
        "Labels": null,
        "Mountpoint": "/var/lib/nvidia-docker/volumes/nvidia_driver/375.66",
        "Name": "nvidia_driver_375.66",
        "Options": {},
        "Scope": "local"
    }
]
```

```console
$ tree /var/lib/nvidia-docker/volumes/nvidia_driver/375.66
/var/lib/nvidia-docker/volumes/nvidia_driver/375.66
├── bin
│   ├── nvidia-cuda-mps-control
│   ├── nvidia-cuda-mps-server
│   ├── nvidia-debugdump
│   ├── nvidia-persistenced
│   └── nvidia-smi
├── lib
│   ├── libEGL_nvidia.so.0 -> libEGL_nvidia.so.375.66
│   ├── libEGL_nvidia.so.375.66
│   ├── libEGL.so.1
│   ├── libGLdispatch.so.0
│   ├── libGLESv1_CM_nvidia.so.1 -> libGLESv1_CM_nvidia.so.375.66
│   ├── libGLESv1_CM_nvidia.so.375.66
│   ├── libGLESv1_CM.so.1
│   ├── libGLESv2_nvidia.so.2 -> libGLESv2_nvidia.so.375.66
│   ├── libGLESv2_nvidia.so.375.66
│   ├── libGLESv2.so.2
│   ├── libGL.so.1 -> libGL.so.1.0.0
│   ├── libGL.so.1.0.0
│   ├── libGLX_indirect.so.0 -> libGLX_nvidia.so.375.66
│   ├── libGLX_nvidia.so.0 -> libGLX_nvidia.so.375.66
│   ├── libGLX_nvidia.so.375.66
│   ├── libGLX.so.0
│   ├── libnvcuvid.so.1 -> libnvcuvid.so.375.66
│   ├── libnvcuvid.so.375.66
│   ├── libnvidia-compiler.so.375.66
│   ├── libnvidia-eglcore.so.375.66
│   ├── libnvidia-egl-wayland.so.375.66
│   ├── libnvidia-encode.so.1 -> libnvidia-encode.so.375.66
│   ├── libnvidia-encode.so.375.66
│   ├── libnvidia-fatbinaryloader.so.375.66
│   ├── libnvidia-fbc.so.1 -> libnvidia-fbc.so.375.66
│   ├── libnvidia-fbc.so.375.66
│   ├── libnvidia-glcore.so.375.66
│   ├── libnvidia-glsi.so.375.66
│   ├── libnvidia-ifr.so.1 -> libnvidia-ifr.so.375.66
│   ├── libnvidia-ifr.so.375.66
│   ├── libnvidia-ml.so.1 -> libnvidia-ml.so.375.66
│   ├── libnvidia-ml.so.375.66
│   ├── libnvidia-ptxjitcompiler.so.375.66
│   ├── libnvidia-tls.so.375.66
│   ├── libvdpau_nvidia.so.1 -> libvdpau_nvidia.so.375.66
│   └── libvdpau_nvidia.so.375.66
└── lib64
    ├── libcuda.so -> libcuda.so.375.66
    ├── libcuda.so.1 -> libcuda.so.375.66
    ├── libcuda.so.375.66
    ├── libEGL_nvidia.so.0 -> libEGL_nvidia.so.375.66
    ├── libEGL_nvidia.so.375.66
    ├── libEGL.so.1
    ├── libGLdispatch.so.0
    ├── libGLESv1_CM_nvidia.so.1 -> libGLESv1_CM_nvidia.so.375.66
    ├── libGLESv1_CM_nvidia.so.375.66
    ├── libGLESv1_CM.so.1
    ├── libGLESv2_nvidia.so.2 -> libGLESv2_nvidia.so.375.66
    ├── libGLESv2_nvidia.so.375.66
    ├── libGLESv2.so.2
    ├── libGL.so.1 -> libGL.so.1.0.0
    ├── libGL.so.1.0.0
    ├── libGLX_indirect.so.0 -> libGLX_nvidia.so.375.66
    ├── libGLX_nvidia.so.0 -> libGLX_nvidia.so.375.66
    ├── libGLX_nvidia.so.375.66
    ├── libGLX.so.0
    ├── libnvcuvid.so.1 -> libnvcuvid.so.375.66
    ├── libnvcuvid.so.375.66
    ├── libnvidia-compiler.so.375.66
    ├── libnvidia-eglcore.so.375.66
    ├── libnvidia-egl-wayland.so.375.66
    ├── libnvidia-encode.so.1 -> libnvidia-encode.so.375.66
    ├── libnvidia-encode.so.375.66
    ├── libnvidia-fatbinaryloader.so.375.66
    ├── libnvidia-fbc.so.1 -> libnvidia-fbc.so.375.66
    ├── libnvidia-fbc.so.375.66
    ├── libnvidia-glcore.so.375.66
    ├── libnvidia-glsi.so.375.66
    ├── libnvidia-ifr.so.1 -> libnvidia-ifr.so.375.66
    ├── libnvidia-ifr.so.375.66
    ├── libnvidia-ml.so.1 -> libnvidia-ml.so.375.66
    ├── libnvidia-ml.so.375.66
    ├── libnvidia-ptxjitcompiler.so.375.66
    ├── libnvidia-tls.so.375.66
    ├── libOpenGL.so.0
    ├── libvdpau_nvidia.so.1 -> libvdpau_nvidia.so.375.66
    └── libvdpau_nvidia.so.375.66

3 directories, 81 files
```

`/var/lib/nvidia-docker/volumes/nvidia_driver/375.66/bin/` に `nvidia-smi` が置かれているのがわかる。

あとは Dockerfile で `PATH` や `LD_LIBRARY_PATH` が適切に設定されていれば、コンテナから GPU や CUDA が利用できるわけである。
しかしその環境変数を適切に設定するのも面倒なわけで、そこで https://hub.docker.com/r/nvidia/cuda/ にある NVIDIA 公式の docker image が `/usr/local/nvidia/` にあるドライバやライブラリにパスを通している。
例えば `nvidia/cuda:8.0-runtime-ubuntu16.04` イメージの Dockerfile はこのようになっている。

```Dockerfile
FROM ubuntu:16.04
LABEL maintainer "NVIDIA CORPORATION <cudatools@nvidia.com>"

RUN NVIDIA_GPGKEY_SUM=d1be581509378368edeec8c1eb2958702feedf3bc3d17011adbf24efacce4ab5 && \
    NVIDIA_GPGKEY_FPR=ae09fe4bbd223a84b2ccfce3f60f4b3d7fa2af80 && \
    apt-key adv --fetch-keys http://developer.download.nvidia.com/compute/cuda/repos/ubuntu1604/x86_64/7fa2af80.pub && \
    apt-key adv --export --no-emit-version -a $NVIDIA_GPGKEY_FPR | tail -n +5 > cudasign.pub && \
    echo "$NVIDIA_GPGKEY_SUM  cudasign.pub" | sha256sum -c --strict - && rm cudasign.pub && \
    echo "deb http://developer.download.nvidia.com/compute/cuda/repos/ubuntu1604/x86_64 /" > /etc/apt/sources.list.d/cuda.list

ENV CUDA_VERSION 8.0.61
LABEL com.nvidia.cuda.version="${CUDA_VERSION}"
ENV NVIDIA_CUDA_VERSION $CUDA_VERSION

ENV CUDA_PKG_VERSION 8-0=$CUDA_VERSION-1
RUN apt-get update && apt-get install -y --no-install-recommends \
        cuda-nvrtc-$CUDA_PKG_VERSION \
        cuda-nvgraph-$CUDA_PKG_VERSION \
        cuda-cusolver-$CUDA_PKG_VERSION \
        cuda-cublas-8-0=8.0.61.2-1 \
        cuda-cufft-$CUDA_PKG_VERSION \
        cuda-curand-$CUDA_PKG_VERSION \
        cuda-cusparse-$CUDA_PKG_VERSION \
        cuda-npp-$CUDA_PKG_VERSION \
        cuda-cudart-$CUDA_PKG_VERSION && \
    ln -s cuda-8.0 /usr/local/cuda && \
    rm -rf /var/lib/apt/lists/*

# nvidia-docker 1.0
LABEL com.nvidia.volumes.needed="nvidia_driver"

RUN echo "/usr/local/nvidia/lib" >> /etc/ld.so.conf.d/nvidia.conf && \
    echo "/usr/local/nvidia/lib64" >> /etc/ld.so.conf.d/nvidia.conf

ENV PATH /usr/local/nvidia/bin:/usr/local/cuda/bin:${PATH}
ENV LD_LIBRARY_PATH /usr/local/nvidia/lib:/usr/local/nvidia/lib64

# nvidia-container-runtime
ENV NVIDIA_VISIBLE_DEVICES all
ENV NVIDIA_DRIVER_CAPABILITIES compute,utility
```

まず CUDA をインストールして、それから `ENV LD_LIBRARY_PATH` でドライバへのパスを通しているのがわかる。


詳細は以下を参照のこと。

* nvidia-docker wiki - Motivation - https://github.com/NVIDIA/nvidia-docker/wiki/Motivation
* nvidia-docker wiki - Internals - https://github.com/NVIDIA/nvidia-docker/wiki/Internals
* nvidia-docker wiki - NVIDIA-driver - https://github.com/NVIDIA/nvidia-docker/wiki/NVIDIA-driver
* nvidia/cuda - https://hub.docker.com/r/nvidia/cuda/
* nvidia/cuda:8.0-runtime-ubuntu16.04 - https://gitlab.com/nvidia/cuda/blob/ubuntu16.04/8.0/runtime/Dockerfile
* nvidia/cuda:8.0-devel-ubuntu16.04 - https://gitlab.com/nvidia/cuda/blob/ubuntu16.04/8.0/devel/Dockerfile
* nvidia/cuda:8.0-cudnn5-devel-ubuntu16.04 - https://gitlab.com/nvidia/cuda/blob/ubuntu16.04/8.0/devel/cudnn5/Dockerfile


#### nvidia-docker build 時に `/usr/local/nvidia` がない問題

nvidia-docker のこの方法ではプロビジョニング時(docker build時)にはコンテナ内の `/usr/local/nvidia` が存在しない。
そのため、ドライバライブラリに依存するライブラリのビルドは Dockerfile では記述できない場合がある。
例えば OpenCV のビルド時に動画のエンコードデコードを高速化する `libnvcuvid.so` というドライバ付属ライブラリを使いたくても、 configure 時に存在しないのでオプションを有効にすることができない。

ではどうすればいいのかというと OpenCV をビルドする前に一旦 `nvidia-docker build` してイメージを作成してしまう。
それから `nvidia-docker run` で OpenCV をビルドし、 `docker commit` で NVCUVID が有効な OpenCV 入りイメージを再度作成するという手がある。

プロビジョニング時に一旦ドライバをインストールしてから OpenCV をビルドし、最後にドライバを `apt-get purge` すればいいのでは？と思うかもしれないが、 `nvidia/cuda` イメージをベースにしているとドライバのインストールに失敗する。
あるいは Dockerfile でホストのドライバライブラリをコンテナ内へ `COPY` するという手もあるが、煩雑なのでおすすめできない。


### nvidia-docker 2.0
2017年10月現在、 https://github.com/NVIDIA/nvidia-docker/tree/2.0 において nvidia-docker 2.0 ユーティリティが開発されており、
nvidia-docker 2.0 は今まで説明した 1.0 とは異なった実装になっている。

* docker のコンテナランタイムである containerd 技術を直接使っている
* `nvidia-docker` という docker ラッパーコマンドはなくなり、 `docker run --runtime=nvidia` として起動する
* nvidia-docker-plugin デーモンはもはや必要ない
* 公式配布の `nvidia/cuda` イメージだけではなく、任意のイメージに対して GPU サポートを有効にできる
* バックエンドに https://github.com/NVIDIA/nvidia-container-runtime と https://github.com/NVIDIA/libnvidia-container を利用している
* Kubernetes プラグイン https://github.com/NVIDIA/k8s-device-plugin と組み合わせて使えるようになる

いつ正式リリースされるのかは不明であるが、開発状況から見るとおそらく来年春までには何らかの発表があると思われる。


## 導入

この章ではnvidiaドライバ、 docker デーモン、 nvidia-docker コマンドのインストールおよび注意点を説明する。
以後ホスト OS は Ubuntu 16.04 として話を進めていく。

### ホストへの nvidia ドライバのインストール
* 公式ドキュメント - https://help.ubuntu.com/community/BinaryDriverHowto/Nvidia

有効なドライバを確認

```sh
sudo apt install -y --no-install-recommends ubuntu-drivers-common
ubuntu-drivers list
```

ドライバのインストールと再起動

```sh
sudo apt install -y --no-install-recommends nvidia-375 nvidia-modprobe
sudo reboot now
```

### docker のインストール
* 公式ドキュメント - https://docs.docker.com/engine/installation/linux/docker-ce/ubuntu/

```sh
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo apt-key add -
sudo add-apt-repository \
   "deb [arch=amd64] https://download.docker.com/linux/ubuntu \
   $(lsb_release -cs) \
   stable"
sudo apt-get update -y
sudo apt-get install -y docker-ce
```

### nvidia-docker のインストール
* 公式ドキュメント - https://github.com/NVIDIA/nvidia-docker

```sh
wget -P /tmp https://github.com/NVIDIA/nvidia-docker/releases/download/v1.0.1/nvidia-docker_1.0.1-1_amd64.deb
sudo dpkg -i /tmp/nvidia-docker*.deb && rm /tmp/nvidia-docker*.deb
sudo nvidia-docker run --rm nvidia/cuda nvidia-smi
```

このように出れば OK

```console
$ sudo nvidia-docker run --rm nvidia/cuda:8.0-runtime nvidia-smi
Wed Oct 18 09:28:50 2017
+-----------------------------------------------------------------------------+
| NVIDIA-SMI 375.66                 Driver Version: 375.66                    |
|-------------------------------+----------------------+----------------------+
| GPU  Name        Persistence-M| Bus-Id        Disp.A | Volatile Uncorr. ECC |
| Fan  Temp  Perf  Pwr:Usage/Cap|         Memory-Usage | GPU-Util  Compute M. |
|===============================+======================+======================|
|   0  Tesla K80           Off  | 2E50:00:00.0     Off |                    0 |
| N/A   32C    P8    31W / 149W |      0MiB / 11439MiB |      0%      Default |
+-------------------------------+----------------------+----------------------+
|   1  Tesla K80           Off  | 42A1:00:00.0     Off |                    0 |
| N/A   39C    P8    25W / 149W |      0MiB / 11439MiB |      0%      Default |
+-------------------------------+----------------------+----------------------+
                                                                               
+-----------------------------------------------------------------------------+
| Processes:                                                       GPU Memory |
|  GPU       PID  Type  Process name                               Usage      |
|=============================================================================|
|  No running processes found                                                 |
+-----------------------------------------------------------------------------+
```

__※ `nvidia-docker run --rm nvidia/cuda nvidia-smi` したら `Error: unsupported CUDA version: driver 0.0 < image 8.0.61` などが出たとき__

* https://hub.docker.com/r/nvidia/cuda/ のデフォルトのイメージが CUDA 9.0 になったのが原因
* `nvidia-docker run --rm nvidia/cuda:8.0-runtime nvidia-smi` で CUDA 8.0 を指定するとよい
* それでダメなら `sudo apt-get install libcuda1-375` を試す - https://github.com/NVIDIA/nvidia-docker/issues/257#issuecomment-264039866
* それでもダメなときは https://github.com/NVIDIA/nvidia-docker/issues/ で検索


### docker group の追加
* https://docs.docker.com/engine/installation/linux/linux-postinstall/

```sh
sudo groupadd docker
sudo usermod -aG docker $USER
```

再ログインし直すと sudo なしで docker コマンドが使えるようになる。

#### docker グループユーザの root 権限を無効にする

`-v` オプションを使えばホストの root 所有権のディレクトリもコンテナ内へとマウントできてしまう。
例えば、 root ユーザ以外見ることができないはずの `/root` を読み書きすることができてしまう。

```console
$ docker run --rm -v /root:/opt/root ubuntu:16.04 ls -la /opt/root
total 24
drwx------ 3 root root 4096 Oct 17 09:23 .
drwxr-xr-x 1 root root 4096 Oct 18 10:53 ..
-rw------- 1 root root   75 Oct 17 09:23 .bash_history
-rw-r--r-- 1 root root 3106 Oct 22  2015 .bashrc
-rw-r--r-- 1 root root  148 Aug 17  2015 .profile
drwx------ 2 root root 4096 Oct  3 02:32 .ssh
```

この問題を回避するには、 docker デーモンを `--userns-remap` オプション付きで実行すればよい。

```sh
$ sudo systemctl stop docker.service
$ sudo dockerd --userns-remap=ubuntu:ubuntu &
$ docker run --rm -v /root:/opt/root ubuntu:16.04 ls -la /opt/root

ls: cannot open directory '/opt/root': Permission denied
$ kill %1
```

`--userns-remap=ubuntu:ubuntu` オプションにより、コンテナ内では任意のユーザ＆グループのプロセスを、ホスト側からは `ubuntu` グループの `ubuntu` ユーザとして見えるように強制することができる。
この設定は `/etc/docker/daemon.json` に記述すれば永続化できる。

```
$ sudo systemctl stop docker.service
$ sudo vim /etc/docker/daemon.json
{
    "userns-remap": "ubuntu:ubuntu"
}
$ sudo systemctl start docker.service
```

これで docker グループユーザの root 権限能力を無効にすることができた。

以下参考リンク

* https://www.pugetsystems.com/labs/hpc/Docker-and-NVIDIA-docker-on-your-workstation-Setup-User-Namespaces-906/
* https://docs.docker.com/engine/security/userns-remap/#enable-userns-remap-on-the-daemon


## 作業工程

この章では、 NVIDIA ドライバと docker デーモンをインストールした環境で、コンテナ内に CUDA 作業環境を作り、コンテナ内で作業する手順を説明する。
コンテナはいくらでも作ったり消せたりするので、ディレクトリ構成等が壊れそうになってもコンテナを消すだけで済むので、ホスト環境を汚したくない場合に便利。

コンテナを使った作業手順の概略。

1. Dockerfile 作成
2. `nvidia-docker build ... ./` でイメージを作成
3. `nvidia-docker run -ti ... /bin/bash` でコンテナを作成してコンテナ環境のシェルへログイン
4. コンテナ環境を確認して exit してコンテナ環境を一時停止

コンテナ内作業＆ホストでの作業

1. `docker start ...` でコンテナを再開てシェルへログイン
2. コンテナ内シェルで `screen` や `tmux` を起動
3. コンテナ内で何か作業する
  * ctrl-p + ctrl-q で一旦ホストへ戻れる
  * `docker attach ...` でホストからコンテナ内シェルへ戻れる
4. 作業完了したら exit でコンテナを停止する

手順をひとつづつ解説する

### Dockerfile の作成
Dockerfile(作業環境の初期状態の設計図)を作る。


```Dockerfile
# `FROM nvidia/cuda:8.0-cudnn5-devel-ubuntu16.04` でベースにする CUDA 環境を選ぶことができる。
# 利用可能な CUDA 環境の一覧は https://hub.docker.com/r/nvidia/cuda/ にある。
FROM nvidia/cuda:8.0-cudnn5-devel-ubuntu16.04

# 自動アップグレード
ENV DEBIAN_FRONTEND "noninteractive"
RUN apt-get update -y
RUN apt-get -y \
	-o Dpkg::Options::="--force-confdef" \
	-o Dpkg::Options::="--force-confold" dist-upgrade

# 使いたいソフトウェアを入れる
RUN apt-get install -y --no-install-recommends \
	sudo ssh \
	build-essential \
	zsh screen cmake unzip git curl wget vim tree htop \
	python-dev python-pip python-setuptools \
	python3-dev python3-pip python3-setuptools \
	graphviz

# キャッシュを消してイメージを小さくする
RUN apt-get clean -y
RUN apt-get autoremove -y
RUN apt-get update -y
RUN apt-get upgrade -y
RUN apt-get autoremove -y
RUN apt-get autoclean -y
RUN rm -rf /var/lib/apt/lists/* /var/cache/apt/archives/*

# ユーザ設定
# ユーザ ID をパラメータにすることでホストボリュームに対する操作をユーザ権限で実行するようにしている。
# `--userns-remap` の設定が面倒なときに使う。
ARG user_name=ubuntu
ARG user_id=1942
ARG group_name=ubuntu
ARG group_id=1942

RUN groupadd -g ${group_id} ${group_name}
RUN useradd -u ${user_id} -g ${group_id} -d /home/${user_name} --create-home --shell /usr/bin/zsh ${user_name}
RUN echo "${user_name} ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers
RUN chown -R ${user_name}:${group_name} /home/${user_name}
RUN chsh -s /usr/bin/zsh ${user_name}

# 以後 ubuntu ユーザの設定
USER ubuntu
WORKDIR /home/ubuntu
ENV HOME /home/ubuntu

# zsh
RUN bash -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
RUN echo 'shell "/usr/bin/zsh"' >>  /home/ubuntu/.screenrc

ENV LD_LIBRARY_PATH=/usr/local/nvidia/lib:/usr/local/nvidia/lib64:/usr/local/cuda/lib64:$LD_LIBRARY_PATH

WORKDIR $HOME/
```


### イメージの作成
Dockerfileを元にイメージ(作業環境の初期状態)を作る。

Dockerfile を用意したらこのようなディレクトリを用意して、

```console
$ tree
.
└── Dockerfile

0 directories, 1 file
```

初回(このDockerfileを一度も使ったことがない場合)のみ、 image をビルドする。

```sh
sudo nvidia-docker build \
  --build-arg user_id=$UID \
  --build-arg group_id=$GID \
  -t myworkspace-image \
  ./
```

* `-t myworkspace-image` はイメージの名前(tag)
* `--build-arg` でコンテナプロセスのユーザ ID とグループ ID を自分の ID と一致させておくとホスト側のファイルを編集するときにパーミッション管理が楽

プロビジョニング作業が終わったらイメージが正常に作成されかどうかを `sudo docker images` コマンドで確認する。

```sh
$ docker images
REPOSITORY              TAG                 IMAGE ID            CREATED             SIZE
myworkspace-image       latest              747cb2d60bbe        4 seconds ago       148MB
```

### コンテナの作成とログイン
作成した image を元に新しいコンテナ(作業環境)を作り、コンテナ内シェルへ入る。

```sh
sudo nvidia-docker run \
  --name myworkspace \
  --volume="$(pwd):/opt/workdir" \
  --workdir="/opt/workdir" \
  -ti \
  myworkspace \
    /usr/bin/zsh
```

* `docker run` = `docker create` + `docker start`
* `--name myworkspace` - このコンテナ(作業環境)の名前
* `--volume="$(pwd):/opt/workdir" --workdir="/opt/workdir"` - ホストのカレントディレクトリを `/opt/workdir` へマウントする。ホストファイルをコンテナ側から編集したいときに便利。
* `-ti` - `-t` - 疑似ターミナル (pseudo-TTY) を割り当て - `-i` -  コンテナの STDIN にアタッチ。コンテナ内のシェルを使いたい時に使う。 `/bin/bash` でも良い。

### コンテナ内でのターミナルエミュレータの使用
コンテナ内のシェルにログインしたらまず tmux ないし gnu screen を起動することをおすすめする。
学習などを始めてしまうと他の作業ができなくなってしまうため。

もしコンテナ内のシェルで screen や tmux を起動し忘れて何か作業を開始してしまった場合、 

```sh
docker exec -u 0:0 -ti [container id] /bin/bash
```

することで起動中のコンテナへルート権限で入ることができる。

### コンテナ内シェルからの一時脱出

`ctrl-p + ctrl-q` でコンテナ内のシェル(`/usr/bin/zsh`)から抜け出してホスト側のシェルに戻ることができる。
シェルで起動中のプロセスはそのまま実行される。

```sh
sudo docker attach [container id]
```

でコンテナ内シェルへ入ることができる。

### コンテナの停止と再開
シェルから `exit` するとコンテナは停止する。
それまでに作業したコンテナ内のファイルシステム等環境はそのまま保持される。
再びコンテナ環境へ入りたい場合は

```sh
sudo docker start -ai [containerid]
```

とするとシェルを再開して入ることができる。


### コンテナとイメージの一覧、管理、削除
よく使う基本的なコマンドとか

* `docker ps` - 起動中のコンテナの一覧
* `docker ps -a` - (停止中のコンテナも含めた)作成済みのコンテナ一覧
* `docker images` - キャッシュ済みのイメージの一覧
* `docker rm [container id]` - コンテナの削除。CUDAのバージョンが古くなったなどでこのコンテナはもう要らないなという時、ディスク容量が逼迫してきた時に使う。
* `docker rmi [image id]` - イメージの削除。同上。



## 応用

実用上便利なテクニックをいくつか紹介する

###  コンテナ内からホストの USB カメラにアクセスしたい
デバイスファイルを透過する。

```sh
nvidia-docker run \
  --device=/dev/video0:/dev/video0 \
  -ti \
  foo \
    /bin/bash
```

参考リンク

* http://www.itmedia.co.jp/enterprise/articles/1603/02/news031_2.html

### コンテナ内の GUI アプリケーションを動かしたい

xhost をローカルユーザに公開して X11 UNIX ソケット経由で描画する。

```sh
xhost +local:
nvidia-docker run \
  --env="DISPLAY=$DISPLAY" \
  --volume="/tmp/.X11-unix:/tmp/.X11-unix:rw" \
  foo \
    /usr/bin/xeyes
```

参考リンク

* http://wiki.ros.org/docker/Tutorials/GUI

なおリモートホストで動作中の docker コンテナ内の GUI プロセスも `ssh -XC foo@bar` などの x11 forwarding が使えるが、  `ssh -L 5901:localhost:5901 foo@bar` して `tightvnc` 使った方が遅延が少ない気がする。

### コンテナ内で Visual Studio Code を動かしたい
C++ の開発において、依存ライブラリはOS付属のパッケージマネージャ `apt` や `/usr/local/` などで管理している。
この開発環境も docker 内に隔離してしまいたいが、 コード補完やデバッガを使うには `/usr` ディレクトリにアクセスできないといけない。
コンテナ内の bash にログインしてプラグインまみれの vim で開発すればよいのだけれど、どうせなら vscode を使いたい、という奇特な人向け。

```Dockerfile
FROM ubuntu:16.04
ENV DEBIAN_FRONTEND "noninteractive"

RUN apt-get update -y
RUN apt-get -y \
  -o Dpkg::Options::="--force-confdef" \
  -o Dpkg::Options::="--force-confold" dist-upgrade

RUN apt-get install -y --no-install-recommends \
  sudo apt-transport-https software-properties-common ppa-purge apt-utils \
  ca-certificates git curl wget \
  tar zip unzip zlib1g-dev bzip2 libbz2-dev \
  zsh vim screen tree htop

# C++17 が使いたい
RUN wget -O - https://apt.llvm.org/llvm-snapshot.gpg.key | apt-key add -
RUN add-apt-repository "deb http://apt.llvm.org/xenial/ llvm-toolchain-xenial-5.0 main"
RUN add-apt-repository ppa:ubuntu-toolchain-r/test
RUN apt-get update -y
RUN apt-get install -y --no-install-recommends \
  build-essential binutils cmake autoconf automake autogen pkg-config libtool \
  gcc-7 g++-7 gdb \
  clang-5.0 lldb-5.0 lld-5.0
RUN update-alternatives --install /usr/bin/gcc gcc /usr/bin/gcc-7 20
RUN update-alternatives --install /usr/bin/g++ g++ /usr/bin/g++-7 20

# vscode 依存ライブラリ
RUN apt-get install -y --no-install-recommends \
  libgtk2.0-0 libgtk-3-0 libgconf2-4 gvfs-bin \
  libpango-1.0-0 libcairo2 libfontconfig1 gettext aspell aspell-en rxvt-unicode-256color \
  libasound2 libcanberra-gtk-module libgl1-mesa-glx \
  libsecret-1-0 libnss3 libnotify-bin \
  x11-xserver-utils libxkbfile1 libxtst6 libxss1 xterm

RUN wget -O vscode-amd64.deb  https://go.microsoft.com/fwlink/?LinkID=760868
RUN dpkg -i vscode-amd64.deb
RUN rm vscode-amd64.deb

RUN apt-get install -f -y
RUN apt-get update -y
RUN apt-get upgrade -y
RUN apt-get dist-upgrade -y

WORKDIR /tmp
RUN rm -rf /tmp/*
RUN apt-get clean
RUN apt-get autoremove -y
RUN apt-get update -y
RUN apt-get upgrade -y
RUN apt-get autoremove -y
RUN apt-get autoclean -y
RUN rm -rf /var/lib/apt/lists/* /var/cache/apt/archives/*

ARG user_name=ubuntu
ARG user_id=1942
ARG group_name=ubuntu
ARG group_id=1942

RUN groupadd -g ${group_id} ${group_name}
RUN useradd -u ${user_id} -g ${group_id} -d /home/${user_name} --create-home --shell /usr/bin/zsh ${user_name}
RUN echo "${user_name} ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers
RUN chown -R ${user_name}:${group_name} /home/${user_name}
RUN chsh -s /usr/bin/zsh ${user_name}

USER ${user_name}
WORKDIR /home/${user_name}
ENV HOME /home/${user_name}

RUN bash -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
RUN echo 'shell "/usr/bin/zsh"' >>  /home/ubuntu/.screenrc

# vscode plugins
RUN /usr/bin/code --install-extension ms-vscode.cpptools
RUN /usr/bin/code --install-extension vector-of-bool.cmake-tools
RUN /usr/bin/code --install-extension DevonDCarew.bazel-code
RUN /usr/bin/code --install-extension naereen.makefiles-support-for-vscode
RUN /usr/bin/code --install-extension maelvalais.autoconf
```

```sh
sudo docker build \
  --build-arg user_id=$UID \
  --build-arg group_id=$GID \
  -t vscode-image \
  ./
xhost +local:
sudo docker run \
  --rm \
  --env="DISPLAY=$DISPLAY" \
  --volume="/tmp/.X11-unix:/tmp/.X11-unix:rw" \
  --volume="$(pwd):/opt/workdir" \
  --workdir="/opt/workdir" \
  -ti \
  vscode-image \
    /usr/bin/code --verbose ./
```


### コンテナ内の jupyter notebook や tensorboard へアクセスしたい
* コンテナ内で `jupyter notebook --ip=0.0.0.0 --port=8080` として jupyter を起動。
* `http://localhost:8080` にアクセス
* tensorboard の場合は `tensorboard --port=8080 --logdir=log` とか

### prometheus で GPU 使用率を監視したい
https://github.com/roguePanda/nvidia_exporter を使う。

```Dockerfile
FROM nvidia/cuda:8.0-cudnn6-devel-ubuntu16.04

RUN apt-get update
RUN apt-get -y \
  -o Dpkg::Options::="--force-confdef" \
  -o Dpkg::Options::="--force-confold" dist-upgrade
RUN apt-get install  -y --no-install-recommends \
  git python-dev python-numpy python-pip python-setuptools 
RUN rm -rf /var/lib/apt/lists/*

RUN useradd --user-group --create-home --shell /bin/false app

ENV HOME /home/app
WORKDIR $HOME

RUN git clone https://github.com/roguePanda/nvidia_exporter.git && \
  cd nvidia_exporter && \
  git checkout -b aug 9d327cbca06eed0a27966656fdff6827d7559131 && \
  python setup.py install
RUN chown -R app:app $HOME/*

USER app

ENV LD_LIBRARY_PATH=/usr/local/nvidia/lib:/usr/local/nvidia/lib64:/usr/local/cuda/lib64:$LD_LIBRARY_PATH

CMD ["nvidia_exporter", "9200"]
```

```sh
sudo nvidia-docker build -t nvidia_exporter .
sudo nvidia-docker run \
  --rm \
  -p 9200:9200 \
  nvidia_exporter \
    nvidia_exporter 9200
```

### docker-compose したい
#### nvidia-docker のしくみのおさらい
nvidia-docker は

```sh
docker run \
  --volume-driver=nvidia-docker \
  --volume=nvidia_driver_375.66:/usr/local/nvidia:ro \
  foo
```

として起動される。
この `--volume-driver=nvidia-docker` プラグインが `nvidia_driver_361.48` というドライバファイルをまとめたボリュームを動的に作成し、コンテナに適用する。

静的にドライバファイルのボリュームを作るには

```sh
docker volume create \
  --name=nvidia_driver_375.66 \
  --driver=nvidia-docker
```
のように `nvidia_driver_361.48` ドライバボリュームを作っておいてから、

```sh
docker run \
  --volume=nvidia_driver_375.66:/usr/local/nvidia:ro \
  foo
```
としてコンテナに適用して起動できる。

#### docker-compose の場合

docker-compose でも同じことをすればよい。

```sh
docker volume create \
  --name=nvidia_driver_375.66 \
   --driver nvidia-docker
```

として `nvidia_driver_375.66` ドライバボリュームを作っておいてから、 `docker-compose.yml` ファイルを

```yml
version: '3'
volumes:
  nvidia_driver_375.66:
    external: true
services:
  nvidia_exporter:
    image:
    - nvidia/cuda:8.0-runtime
    command:
    - nvidia-smi
    - '-l'
    devices:
    - /dev/nvidia0
    - /dev/nvidiactl
    - /dev/nvidia-uvm
    volumes:
    - nvidia_driver_375.66:/usr/local/nvidia:ro
```

としてデバイスファイルとドライバをコンテナに渡せばよい。


#### nvidia-docker-compose
https://github.com/eywalker/nvidia-docker-compose というツールを使うと

```yml
version: '3'
services:
  nvidia_exporter:
    image:
    - nvidia/cuda:8.0-runtime
    command:
    - nvidia-smi
    - '-l'
```

のような GPU 環境非依存の `docker-compose.yml` ファイルから上のような `nvidia-docker` 適用済み `docker-compose.yml` ファイルを作成してくれる。

`nvidia-docker-compose` コマンドは `docker-compose` コマンドのラッパとしても使える

```sh
nvidia-docker-compose build
nvidia-docker-compose up
```


参考リンク

* https://stackoverflow.com/questions/43368470/use-nvidia-docker-from-docker-compose
* https://gist.github.com/cgarciae/2ab3642a8a7b33843b964b3210ac2120
* https://stackoverflow.com/questions/41346401/use-nvidia-docker-compose-launch-a-container-but-exited-soon
* https://gist.github.com/cgarciae/2ab3642a8a7b33843b964b3210ac2120
* https://github.com/NVIDIA/nvidia-docker/wiki/NVIDIA-driver#alternatives

### docker-machine + azure したい

```sh
docker-machine create \
  --driver azure \
  --azure-environment AzurePublicCloud \
  --azure-location westus2 \
  --azure-subscription-id xx-xx-xx-xx-xx \
  --azure-client-id xx-xx-xx-xx-xx \
  --azure-client-secret xx-xx-xx-xx-xx \
  --azure-image canonical:UbuntuServer:16.04.0-LTS:latest \
  --azure-size Standard_NC6 \
  --azure-resource-group rancher-gpu \
  --engine-install-url https://gist.githubusercontent.com/legokichi/xxxx/raw/nvidia-docker-install.sh \
  $MACHINE_NAME
```

* docker-machine rm $MACHINE_NAME で消さないとリソースが増えすぎて面倒なことになるので注意。
* docker-machine env $MACHINE_NAME で環境変数見る
* docker-machine ssh $MACHINE_NAME で ssh で入る
* $MACHINE_NAME は rancher の GUI 上で表示されるインスタンス名で、azure の仮想マシンの名前はまた別に命名されます。 azure portak と見比べるときは ip などで判断してください
* docker-machine azure-driver のオプション一覧 - https://docs.docker.com/machine/drivers/azure/#options
* `--azure-subscription-id` - azure portal のサブスクリプションで見えます
* `--azure-client-id` と `--azure-client-secret` はここ - https://docs.microsoft.com/en-us/azure/container-service/kubernetes/container-service-kubernetes-service-principal
* 権限とリソースを指定してアプリを作成し、キーを発行、 appId が client_id, password が client_secret
* 
```sh
az ad sp create-for-rbac --name rancher --role=Owner
{
  "appId": "xxxxxxx",
  "displayName": "rancher",
  "name": "http://rancher",
  "password": "xxxx",
  "tenant": "xxxxx"
}
```

* `--azure-location` と `--azure-size` - このへんを参照
  * https://docs.microsoft.com/ja-jp/azure/virtual-machines/windows/sizes?toc=%2Fazure%2Fvirtual-machines%2Fwindows%2Ftoc.json
  * https://docs.microsoft.com/ja-jp/azure/virtual-machines/windows/sizes-general
  * https://docs.microsoft.com/ja-jp/azure/virtual-machines/windows/sizes-gpu
  * https://docs.microsoft.com/ja-jp/azure/virtual-machines/windows/sizes-compute
* `--azure-image` - nvidia-docker 使いたい場合は基本的にホストマシンは ubuntu16 固定で
* `--azure-resource-group` - 時節に応じて適切な名前を
* `--engine-install-url` - docker と nvidia-docker をインストールする添付のスクリプト。

### rancher に nvidia-docker ホストを追加したい

rancher 1.x の GUI では現状 azure の NC|NV GPU インスタンスを選択することができない。
そこで rancher-cli を使う。

```sh
rancher host create \
  --name test-azure-nvidia-host \
  --azure-open-port=500/udp \
  --azure-open-port=4500/udp \
  --driver azure \
  --azure-environment AzurePublicCloud \
  --azure-location westus2 \
  --azure-subscription-id xx-xx-xx-xx-xx \
  --azure-client-id xx-xx-xx-xx-xx \
  --azure-client-secret xx-xx-xx-xx-xx \
  --azure-image canonical:UbuntuServer:16.04.0-LTS:latest \
  --azure-size Standard_NC6 \
  --azure-resource-group rancher \
  --azure-availability-set docker-machine \
  --engine-install-url https://gist.githubusercontent.com/legokichi/xxxxx/raw/nvidia-docker-install.sh
```

インストールスクリプトは以下のように書く。

```sh:nvidia-docker-install.sh
#!/bin/bash
set -eu

# nvidia-driver
sudo apt-key adv --fetch-keys http://developer.download.nvidia.com/compute/cuda/repos/ubuntu1604/x86_64/7fa2af80.pub
sudo sh -c 'echo "deb http://developer.download.nvidia.com/compute/cuda/repos/ubuntu1604/x86_64 /" > /etc/apt/sources.list.d/cuda.list'
sudo apt-get update -y
sudo apt-cache madison cuda-drivers
sudo apt-get install -y --no-install-recommends cuda-drivers=384.81-1

# docker
sudo apt-get install -y \
  apt-transport-https \
  ca-certificates \
  curl \
  software-properties-common
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo apt-key add -
sudo add-apt-repository \
   "deb [arch=amd64] https://download.docker.com/linux/ubuntu \
   $(lsb_release -cs) \
   stable"
sudo apt-get update -y
sudo apt-cache madison docker-ce
sudo apt-get install -y docker-ce=17.06.2~ce-0~ubuntu

# nvidia-docker
wget -P /tmp https://github.com/NVIDIA/nvidia-docker/releases/download/v1.0.1/nvidia-docker_1.0.1-1_amd64.deb
sudo dpkg -i /tmp/nvidia-docker*.deb && rm /tmp/nvidia-docker*.deb

# driver hack
export NVIDIA_VERSION=`nvidia-smi --query | grep "Driver Version" | head -n 1 | cut -d ':' -f 2 | awk '{$1=$1;print}'`
sudo echo $NVIDIA_VERSION | sudo tee /opt/check

sudo docker volume create --name=nvidia_driver_$NVIDIA_VERSION -d nvidia-docker
export NVIDIA_DRIVER_DIR=`sudo docker volume inspect -f "{{ .Mountpoint }}" nvidia_driver_${NVIDIA_VERSION}`
sudo echo $NVIDIA_DRIVER_DIR | sudo tee -a /opt/check
sudo echo "`sudo ls -la $NVIDIA_DRIVER_DIR`" | sudo tee -a /opt/check

sudo mkdir -p /usr/local/nvidia/
sudo cp -ra $NVIDIA_DRIVER_DIR/* /usr/local/nvidia/ 2>&1 | sudo tee -a /opt/check
```

nvidia-volume によるドライバのバージョン管理ではボリューム名にドライバのバージョン番号が入ってしまうため docker-compose ファイルを書くのが面倒になる。
そこでホストの `/usr/local/nvidia/` にすべてのドライバを入れてしまい、nvidia-dockerの管理外のボリュームを作成している。

docker-compose ファイルにドライバのバージョン番号を書かなくても良くなる。

```sh
version: '2'
services:
  nvidia-smi:
    image: nvidia/cuda:8.0-runtime
    command: /usr/bin/env nvidia-smi -l
    devices:
    - /dev/nvidia0
    - /dev/nvidiactl
    - /dev/nvidia-uvm
    - /dev/nvidia-uvm-tools
    volumes:
    - /usr/local/nvidia/:/usr/local/nvidia:ro
```

インストールスクリプトの docker と cuda-driver のバージョンを調べるには下記のようにする

```sh
$ apt-cache madison cuda-drivers
cuda-drivers |   384.81-1 | http://developer.download.nvidia.com/compute/cuda/repos/ubuntu1604/x86_64  Packages
cuda-drivers |   384.66-1 | http://developer.download.nvidia.com/compute/cuda/repos/ubuntu1604/x86_64  Packages
cuda-drivers |   375.88-1 | http://developer.download.nvidia.com/compute/cuda/repos/ubuntu1604/x86_64  Packages
cuda-drivers |   375.74-1 | http://developer.download.nvidia.com/compute/cuda/repos/ubuntu1604/x86_64  Packages
cuda-drivers |   375.51-1 | http://developer.download.nvidia.com/compute/cuda/repos/ubuntu1604/x86_64  Packages
cuda-drivers |   375.26-1 | http://developer.download.nvidia.com/compute/cuda/repos/ubuntu1604/x86_64  Packages
cuda-drivers |   367.48-1 | http://developer.download.nvidia.com/compute/cuda/repos/ubuntu1604/x86_64  Packages
$ apt-cache madison docker-ce
docker-ce | 17.09.0~ce-0~ubuntu | https://download.docker.com/linux/ubuntu xenial/stable amd64 Packages
docker-ce | 17.06.2~ce-0~ubuntu | https://download.docker.com/linux/ubuntu xenial/stable amd64 Packages
docker-ce | 17.06.1~ce-0~ubuntu | https://download.docker.com/linux/ubuntu xenial/stable amd64 Packages
docker-ce | 17.06.0~ce-0~ubuntu | https://download.docker.com/linux/ubuntu xenial/stable amd64 Packages
docker-ce | 17.03.2~ce-0~ubuntu-xenial | https://download.docker.com/linux/ubuntu xenial/stable amd64 Packages
docker-ce | 17.03.1~ce-0~ubuntu-xenial | https://download.docker.com/linux/ubuntu xenial/stable amd64 Packages
docker-ce | 17.03.0~ce-0~ubuntu-xenial | https://download.docker.com/linux/ubuntu xenial/stable amd64 Packages
```



#### rancher ホスト用 nvidia-docker 入り AMI の作成

`g3.4xlarge` GPU インスタンス環境で `ubuntu/images/hvm-ssd/ubuntu-xenial-16.04-amd64-server-20170721 (ami-ea4eae8c)` をベースにインスタンスを立てて、 NVIDIA ドライバと docker と nvidia-docker を入れる。

```sh
sudo apt-get update -y
sudo apt install -y --no-install-recommends ubuntu-drivers-common
ubuntu-drivers list
sudo apt install -y --no-install-recommends nvidia-375 nvidia-modprobe
sudo apt install -y libcuda1-375
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo apt-key add -
sudo add-apt-repository \
   "deb [arch=amd64] https://download.docker.com/linux/ubuntu \
   $(lsb_release -cs) \
   stable"
sudo apt-get update -y
sudo apt-get install -y docker-ce
wget -P /tmp https://github.com/NVIDIA/nvidia-docker/releases/download/v1.0.1/nvidia-docker_1.0.1-1_amd64.deb
sudo dpkg -i /tmp/nvidia-docker*.deb && rm /tmp/nvidia-docker*.deb
# sudo nvidia-docker run --rm nvidia/cuda:8.0-runtime nvidia-smi
sudo apt update -y
sudo apt upgrade -y
sudo apt autoremove -y
sudo apt autoclean -y
sudo rm -rf /var/lib/apt/lists/
```

インスタンスを停止し、 AWS コンソールから AMI を作成する。

#### azure-cli で仮想マシンのディスクイメージから azure 上で rancher ホストを起動する
* azure-cli - https://docs.microsoft.com/en-us/cli/azure/vm?view=azure-cli-latest

インスタンス作成

```sh
time az vm create \
  --resource-group rancher-gpu-host-vmss \
  --name rancher-gpu \
  --location westus2 \
  --size Standard_NC6 \
  --image /subscriptions/xxx/resourceGroups/rancher-gpu-host-vmss/providers/Microsoft.Compute/images/xxx \
  --authentication-type password \
  --admin-username ubuntu \
  --admin-password xxxx
```

azure vm 拡張機能セット用インストールスクリプトで rancher-agent を起動する

```sh
az vm extension set \
  --resource-group rancher-gpu-host-vmss \
  --vm-name rancher-gpu \
  --name CustomScript \
  --publisher Microsoft.Azure.Extensions \
  --version 2.0 \
  --settings '{"fileUris": ["https://gist.githubusercontent.com/legokichi/xxx/raw/automate.sh"],"commandToExecute": "./automate.sh"}'
```

rancher ホストの起動スクリプトに rancherサーバの URL を引数で設定しておく。

```sh:automate.sh
sudo docker run --rm --privileged -v /var/run/docker.sock:/var/run/docker.sock -v /var/lib/rancher:/var/lib/rancher rancher/agent:v1.2.6 http://***
```

```sh:nvidia-docker-install.sh
#!/bin/bash
set -eu

# nvidia-driver
sudo apt-key adv --fetch-keys http://developer.download.nvidia.com/compute/cuda/repos/ubuntu1604/x86_64/7fa2af80.pub
sudo sh -c 'echo "deb http://developer.download.nvidia.com/compute/cuda/repos/ubuntu1604/x86_64 /" > /etc/apt/sources.list.d/cuda.list'
sudo apt-get update -y
sudo apt-cache madison cuda-drivers
sudo apt-get install -y --no-install-recommends cuda-drivers=384.81-1

# docker
sudo apt-get install -y \
  apt-transport-https \
  ca-certificates \
  curl \
  software-properties-common
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo apt-key add -
sudo add-apt-repository \
   "deb [arch=amd64] https://download.docker.com/linux/ubuntu \
   $(lsb_release -cs) \
   stable"
sudo apt-get update -y
sudo apt-cache madison docker-ce
sudo apt-get install -y docker-ce=17.06.2~ce-0~ubuntu

# nvidia-docker
wget -P /tmp https://github.com/NVIDIA/nvidia-docker/releases/download/v1.0.1/nvidia-docker_1.0.1-1_amd64.deb
sudo dpkg -i /tmp/nvidia-docker*.deb && rm /tmp/nvidia-docker*.deb

# driver hack
export NVIDIA_VERSION=`nvidia-smi --query | grep "Driver Version" | head -n 1 | cut -d ':' -f 2 | awk '{$1=$1;print}'`
sudo echo $NVIDIA_VERSION | sudo tee /opt/check

sudo docker volume create --name=nvidia_driver_$NVIDIA_VERSION -d nvidia-docker
export NVIDIA_DRIVER_DIR=`sudo docker volume inspect -f "{{ .Mountpoint }}" nvidia_driver_${NVIDIA_VERSION}`
sudo echo $NVIDIA_DRIVER_DIR | sudo tee -a /opt/check
sudo echo "`sudo ls -la $NVIDIA_DRIVER_DIR`" | sudo tee -a /opt/check

sudo mkdir -p /usr/local/nvidia/
sudo cp -ra $NVIDIA_DRIVER_DIR/* /usr/local/nvidia/ 2>&1 | sudo tee -a /opt/check
```

##### azure VMSS の場合
* azure-cli vmss のドキュメント - https://docs.microsoft.com/en-us/cli/azure/vmss?view=azure-cli-latest
* リソース グループの作成

```sh
az group create \
  --location westus2 \
  --name rancher-gpu-host-vmss
```

* カスタム VM イメージの構築 - https://docs.microsoft.com/ja-jp/azure/virtual-machine-scale-sets/virtual-machine-scale-sets-deploy-app#build-a-custom-vm-image
* スケールセットの作成 - https://docs.microsoft.com/ja-jp/azure/virtual-machine-scale-sets/virtual-machine-scale-sets-create#create-from-azure-cli

```sh
az vmss create \
  --resource-group rancher-gpu-host-vmss \
  --name rancher-gpu \
  --image /subscriptions/xxxx/resourceGroups/rancher-gpu-host-vmss/providers/Microsoft.Compute/images/xxxx-xxxx \
  --authentication-type password \
  --admin-username ubuntu \
  --admin-password xxxx
```

* 情報表示

```sh
az vmss list-instance-connection-info \
  --resource-group rancher-gpu-host-vmss \
  --name rancher-gpu
```

* カスタム スクリプト拡張機能を使用してアプリケーションをインストールする - https://docs.microsoft.com/ja-jp/azure/virtual-machine-scale-sets/virtual-machine-scale-sets-deploy-app#already-provisioned

```sh
az vmss extension set \
  --resource-group rancher-gpu-host-vmss \
  --vmss-name rancher-gpu \
  --name CustomScript \
  --publisher Microsoft.Azure.Extensions \
  --version 2.0 \
  --settings '{"fileUris": ["https://gist.githubusercontent.com/legokichi/xxxx/raw/automate.sh"],"commandToExecute": "./automate.sh"}'
```

* 詳細情報

```sh
az vmss extension show \
  --resource-group rancher-gpu-host-vmss \
  --vmss-name rancher-gpu \
  --name CustomScript
```

#### rancher server で GPU ホストを追加する

rancher server の Add Host から AWS を選択する。region 入力欄が作成した AMI のリージョンと一致するように注意する。

GPU インスタンスを選択し、 SSH User を `ubuntu` に設定する（ベースイメージに`ubuntu-xenial-16.04-amd64-server-20170721`を選択したため）。

ボリュームドライバに `nvidia-docker` と入力し、デバイスに `/dev/nvidia0`, `/dev/nvidiactl`, `/dev/nvidia-uvm` を渡す。
ここでボリュームドライバを指定しているので `docker volume create` はしなくて良い。

プロビジョニングに成功して「ホストの docker のバージョンが違います」と言われても気にしない。




## 小技

### ホスト GPU の確認

```console
$ lspci -vnn | grep -i VGA -A 12
0000:00:08.0 VGA compatible controller [0300]: Microsoft Corporation Hyper-V virtual VGA [1414:5353] (prog-if 00 [VGA controller])
	Flags: bus master, fast devsel, latency 0, IRQ 11
	Memory at f8000000 (32-bit, non-prefetchable) [size=64M]
	[virtual] Expansion ROM at 000c0000 [disabled] [size=128K]
	Kernel driver in use: hyperv_fb
	Kernel modules: hyperv_fb

61f0:00:00.0 3D controller [0302]: NVIDIA Corporation GK210GL [Tesla K80] [10de:102d] (rev a1)
	Subsystem: NVIDIA Corporation GK210GL [Tesla K80] [10de:106c]
	Flags: bus master, fast devsel, latency 0, IRQ 24
	Memory at 22000000 (32-bit, non-prefetchable) [size=16M]
	Memory at 1800000000 (64-bit, prefetchable) [size=16G]
	Memory at 1c00000000 (64-bit, prefetchable) [size=32M]
```

```console
$ lspci | grep -i nvidia
61f0:00:00.0 3D controller: NVIDIA Corporation GK210GL [Tesla K80] (rev a1)
b63b:00:00.0 3D controller: NVIDIA Corporation GK210GL [Tesla K80] (rev a1)
```

### ホストのドライバの確認

```console
$ cat /proc/driver/nvidia/version
NVRM version: NVIDIA UNIX x86_64 Kernel Module  375.66  Mon May  1 15:29:16 PDT 2017
GCC version:  gcc version 5.4.0 20160609 (Ubuntu 5.4.0-6ubuntu1~16.04.5) 
```

```console
$ dpkg -l | head -n 5; dpkg -l | grep nvidia
Desired=Unknown/Install/Remove/Purge/Hold
| Status=Not/Inst/Conf-files/Unpacked/halF-conf/Half-inst/trig-aWait/Trig-pend
|/ Err?=(none)/Reinst-required (Status,Err: uppercase=bad)
||/ Name                                  Version                                    Architecture Description
+++-=====================================-==========================================-============-===============================
ii  nvidia-375                            375.66-0ubuntu0.16.04.1                    amd64        NVIDIA binary driver - version 375.66
ii  nvidia-docker                         1.0.1-1                                    amd64        NVIDIA Docker container tools
ii  nvidia-modprobe                       361.28-1                                   amd64        utility to load NVIDIA kernel modules and create device nodes
ii  nvidia-opencl-icd-375                 375.66-0ubuntu0.16.04.1                    amd64        NVIDIA OpenCL ICD
ii  nvidia-prime                          0.8.2                                      amd64        Tools to enable NVIDIA's Prime
ii  nvidia-settings                       361.42-0ubuntu1                            amd64        Tool for configuring the NVIDIA graphics driver
```

### ホストの利用可能なドライバの一覧

```console
$ ubuntu-drivers devices
== /sys/devices/LNXSYSTM:00/LNXSYBUS:00/PNP0A03:00/device:07/VMBUS:01/22668ee3-15dd-4b02-990b-ba398e1c43cf/pci61f0:00/61f0:00:00.0 ==
modalias : pci:v000010DEd0000102Dsv000010DEsd0000106Cbc03sc02i00
vendor   : NVIDIA Corporation
model    : GK210GL [Tesla K80]
driver   : nvidia-375 - distro non-free recommended
driver   : xserver-xorg-video-nouveau - distro free builtin

== cpu-microcode.py ==
driver   : intel-microcode - distro non-free
```

```console
$ ubuntu-drivers list
intel-microcode
nvidia-375
```

```console
$ apt-cache search nvidia-3
nvidia-304-dev - NVIDIA binary Xorg driver development files
nvidia-331 - Transitional package for nvidia-331
nvidia-331-dev - Transitional package for nvidia-340-dev
nvidia-331-updates - Transitional package for nvidia-340
nvidia-331-updates-dev - Transitional package for nvidia-340-dev
nvidia-331-updates-uvm - Transitional package for nvidia-340
nvidia-331-uvm - Transitional package for nvidia-340
nvidia-340-dev - NVIDIA binary Xorg driver development files
nvidia-340-updates - Transitional package for nvidia-340
nvidia-340-updates-dev - Transitional package for nvidia-340-dev
nvidia-340-updates-uvm - Transitional package for nvidia-340-updates
nvidia-340-uvm - Transitional package for nvidia-340
nvidia-346 - Transitional package for nvidia-346
nvidia-346-dev - Transitional package for nvidia-352-dev
nvidia-346-updates - Transitional package for nvidia-346-updates
nvidia-346-updates-dev - Transitional package for nvidia-352-updates-dev
nvidia-352 - Transitional package for nvidia-361
nvidia-352-dev - Transitional package for nvidia-361-dev
nvidia-352-updates - Transitional package for nvidia-361
nvidia-352-updates-dev - Transitional package for nvidia-361-dev
nvidia-361-updates - Transitional package for nvidia-361
nvidia-361-updates-dev - Transitional package for nvidia-361-dev
nvidia-304 - NVIDIA legacy binary driver - version 304.135
nvidia-304-updates - Transitional package for nvidia-304
nvidia-304-updates-dev - Transitional package for nvidia-304-dev
nvidia-340 - NVIDIA binary driver - version 340.102
nvidia-361 - Transitional package for nvidia-367
nvidia-361-dev - Transitional package for nvidia-367-dev
nvidia-367 - Transitional package for nvidia-375
nvidia-367-dev - Transitional package for nvidia-375-dev
nvidia-375 - NVIDIA binary driver - version 375.66
nvidia-375-dev - NVIDIA binary Xorg driver development files
```

### ホスト Linux の環境の確認

```console
$ uname -a
Linux somehost 4.11.0-1013-azure #13-Ubuntu SMP Mon Oct 2 17:59:06 UTC 2017 x86_64 x86_64 x86_64 GNU/Linux
```
 
```console
$ cat /etc/*release
DISTRIB_ID=Ubuntu
DISTRIB_RELEASE=16.04
DISTRIB_CODENAME=xenial
DISTRIB_DESCRIPTION="Ubuntu 16.04.3 LTS"
NAME="Ubuntu"
VERSION="16.04.3 LTS (Xenial Xerus)"
ID=ubuntu
ID_LIKE=debian
PRETTY_NAME="Ubuntu 16.04.3 LTS"
VERSION_ID="16.04"
HOME_URL="http://www.ubuntu.com/"
SUPPORT_URL="http://help.ubuntu.com/"
BUG_REPORT_URL="http://bugs.launchpad.net/ubuntu/"
VERSION_CODENAME=xenial
UBUNTU_CODENAME=xenial
```

## CUDA 環境の確認

ホストでは cuda をインストールしていない。

```console
$ nvcc -V
The program 'nvcc' is currently not installed. You can install it by typing:
sudo apt install nvidia-cuda-toolkit
```

コンテナの中では確認できる。

```console
$ sudo docker run --rm -ti nvidia/cuda:8.0-cudnn5-devel-ubuntu16.04 nvcc -V
nvcc: NVIDIA (R) Cuda compiler driver
Copyright (c) 2005-2016 NVIDIA Corporation
Built on Tue_Jan_10_13:22:03_CST_2017
Cuda compilation tools, release 8.0, V8.0.61
```

```console
$ nvidia-docker run --rm -ti nvidia/cuda:8.0-cudnn5-devel-ubuntu16.04 bash -c "dpkg -l | head -n 5; dpkg -l | grep cuda"
Desired=Unknown/Install/Remove/Purge/Hold
| Status=Not/Inst/Conf-files/Unpacked/halF-conf/Half-inst/trig-aWait/Trig-pend
|/ Err?=(none)/Reinst-required (Status,Err: uppercase=bad)
||/ Name                        Version                       Architecture Description
+++-===========================-=============================-============-===============================================================================
ii  cuda-command-line-tools-8-0 8.0.61-1                      amd64        CUDA command-line tools
ii  cuda-core-8-0               8.0.61-1                      amd64        CUDA core tools
ii  cuda-cublas-8-0             8.0.61.2-1                    amd64        CUBLAS native runtime libraries
ii  cuda-cublas-dev-8-0         8.0.61.2-1                    amd64        CUBLAS native dev links, headers
ii  cuda-cudart-8-0             8.0.61-1                      amd64        CUDA Runtime native Libraries
ii  cuda-cudart-dev-8-0         8.0.61-1                      amd64        CUDA Runtime native dev links, headers
ii  cuda-cufft-8-0              8.0.61-1                      amd64        CUFFT native runtime libraries
ii  cuda-cufft-dev-8-0          8.0.61-1                      amd64        CUFFT native dev links, headers
ii  cuda-curand-8-0             8.0.61-1                      amd64        CURAND native runtime libraries
ii  cuda-curand-dev-8-0         8.0.61-1                      amd64        CURAND native dev links, headers
ii  cuda-cusolver-8-0           8.0.61-1                      amd64        CUDA solver native runtime libraries
ii  cuda-cusolver-dev-8-0       8.0.61-1                      amd64        CUDA solver native dev links, headers
ii  cuda-cusparse-8-0           8.0.61-1                      amd64        CUSPARSE native runtime libraries
ii  cuda-cusparse-dev-8-0       8.0.61-1                      amd64        CUSPARSE native dev links, headers
ii  cuda-driver-dev-8-0         8.0.61-1                      amd64        CUDA Driver native dev stub library
ii  cuda-license-8-0            8.0.61-1                      amd64        CUDA licenses
ii  cuda-misc-headers-8-0       8.0.61-1                      amd64        CUDA miscellaneous headers
ii  cuda-npp-8-0                8.0.61-1                      amd64        NPP native runtime libraries
ii  cuda-npp-dev-8-0            8.0.61-1                      amd64        NPP native dev links, headers
ii  cuda-nvgraph-8-0            8.0.61-1                      amd64        NVGRAPH native runtime libraries
ii  cuda-nvgraph-dev-8-0        8.0.61-1                      amd64        NVGRAPH native dev links, headers
ii  cuda-nvml-dev-8-0           8.0.61-1                      amd64        NVML native dev links, headers
ii  cuda-nvrtc-8-0              8.0.61-1                      amd64        NVRTC native runtime libraries
ii  cuda-nvrtc-dev-8-0          8.0.61-1                      amd64        NVRTC native dev links, headers
ii  libcudnn5                   5.1.10-1+cuda8.0              amd64        cuDNN runtime libraries
ii  libcudnn5-dev               5.1.10-1+cuda8.0              amd64        cuDNN development libraries and headers
```

### CPU や GPU の使用率が見たい
* `nvidia-smi` - GPU 使用率と使用プロセスのモニタリング。ドライバについてくるのでホストでもコンテナ内でも使える
* `htop` - CPU、メモリ、プロセス一覧のモニタリング。 `apt install htop` でインストール。ホスト、コンテナ内両方で重宝する。

### コンテナとホストの間でファイル転送
* `docker cp foo.txt mycontainer:/foo.txt` ホスト->コンテナ
* `docker cp mycontainer:/foo.txt foo.txt` コンテナ->ホスト

### マルチ GPU 環境でコンテナに割り当てる GPU を制限したい
ホストやコンテナ内では、一般的には
```sh
env CUDA_VISIBLE_DEVICES=0 some_process
```
としてプロセスから見える GPU を制限する。

コンテナ内そのものからアクセスできる GPU 数を制限する場合は `NV_GPU` 環境変数を使う。
 
ホスト環境は GPU ふたつ。

```console
$ nvidia-smi -L
GPU 0: Tesla K80 (UUID: GPU-ed0c67d5-5f87-af95-d4a3-c08805247462)
GPU 1: Tesla K80 (UUID: GPU-e431f402-0937-964d-63bc-78f9d875928a)
```

`NV_GPU=0` で コンテナに割り当てる GPU 数を決める。

```console
$ env NV_GPU='0' nvidia-docker run --rm -ti nvidia/cuda:8.0-cudnn5-devel-ubuntu16.04 nvidia-smi -L
GPU 0: Tesla K80 (UUID: GPU-ed0c67d5-5f87-af95-d4a3-c08805247462)
```

参考

* https://github.com/NVIDIA/nvidia-docker/wiki/nvidia-docker#gpu-isolation



### 未使用のコンテナやイメージの削除

停止済みコンテナ削除

```sh
docker container prune
```

タグ無しイメージ削除

```sh
docker image prune
```

### docker commit で 作業後のコンテナをイメージに固める

1. CTRL-p+CTRL-q をコンソールで入力して、コンテナをdetachする
2. `docker ps` で 今detachしたコンテナのIDを調べる
3. `docker commit -m "comment" <container id> <image name>` で commit
4. `docker images` で ID を確認
5. `docker attach <container id>` で確認

### docker save でイメージをシングルファイルに固める
ファイルに保存

```ssh
sudo docker save -o <save image to file path> <image name>
```

scp 等でイメージファイルを移動した後、ロード

```
sudo docker load -i <path to image file>
```
