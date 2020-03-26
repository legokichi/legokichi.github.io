## Dockerfile内で libnvcuvid.so がない

libnvcuvid.so はドライバ付属ライブラリである。
プロビジョニング時にはまだドライバが配置されていない。

```shell-session
cp /usr/lib/nvidia-375/libnvcuvid.so ./
sudo  nvidia-docker build -t foo ./
```

```Dockerfile
COPY libnvcuvid.so /usr/local/lib
```


などするとよいかも


* https://github.com/NVIDIA/nvidia-docker/issues/103
* https://stackoverflow.com/questions/44131890/nvidia-docker-cant-find-libnvcuvid-so-on-building-process
