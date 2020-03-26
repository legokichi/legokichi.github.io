nvidia-docker2 で libnvcuvid.so.1 がない場合

`-e NVIDIA_VISIBLE_DEVICES=all -e NVIDIA_DRIVER_CAPABILITIES="video,compute,utility"` をつける

https://github.com/NVIDIA/nvidia-docker/issues/531
