* spectre/meltdown 脆弱性 の nvidia driver のパッチ - https://usn.ubuntu.com/usn/usn-3521-1/ - ubuntu16 で dist-upgrade してカーネルアップデートすると 公式の cuda-driver - https://developer.nvidia.com/cuda-toolkit-archive が使えなくなる気がする・・・？
* http://news.softpedia.com/news/canonical-releases-ubuntu-kernel-and-nvidia-updates-to-fix-meltdown-and-spectre-519305.shtml

```sh
sudo apt purge cuda-driver
sudo apt install nvidia-384 nvidia-modprobe
```

* nvidiaの公式特設ページとドライバアップデートの現状
 -
 http://nvidia.custhelp.com/app/answers/detail/a_id/4611
