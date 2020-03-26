# clang 32bit コンパイル で /usr/bin/ld: fatal error: failed to allocate memory

## 問題

ubuntu 14.04 32bit で `./emsdk install clang-incoming-32bit` しようとしたところ、`emsdk_portable/clang/fastcomp/build_incoming_32/bin/clang-3.9` のリンク時に `/usr/bin/ld: fatal error: failed to allocate memory` というエラーを吐いて止まった。

## 原因

32bit OS では 4GB までしかメモリを使えないのに 32bit clang のコンパイルに 4GB 以上メモリ使っている。

## 解決策

`ld` の [-no-keep-memory](https://linuxjm.osdn.jp/html/GNU_binutils/man1/ld.1.html`) オプションを強制的に使わせるために ld　の代わりの shell script を書く。

```
mv /usr/bin/ld /usr/bin/ld.old
echo '/usr/bin/ld.old --no-keep-memory $@' > /usr/bin/ld
chmod a+x /usr/bin/ld
```

これでリンクできた。


```
rm /usr/bin/ld
mv /usr/bin/ld.old /usr/bin/ld
```

終わったら元に戻しておく。

## 参考

* http://stackoverflow.com/questions/33551473/ld-fatal-error-failed-to-allocate-memory
* http://stackoverflow.com/questions/25197570/llvm-clang-compile-error-with-memory-exhausted
* https://linuxjm.osdn.jp/html/GNU_binutils/man1/ld.1.html
