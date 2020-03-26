# netcat で tcp を bash から叩く

netcat はシェルの標準入出力から TCP|UDP を叩くことができるコマンドです。
HTTP サーバや HTTP クライアントも作ることができます。
`-U` オプションを使えば unix domain socket も叩くことができます。

## HTTP サーバ

```bash
#!/bin/bash
nc -l 8080 -q0 << 'EOF'
HTTP/1.1 200 Ok
Connection: close
content-type: plain/text
Hello World

EOF
```
`-q0` で STDIN の EOF を検出したら0秒で終了します

## HTTP クライアント

```bash
#!/bin/bash
nc localhost 8080 -q1 <<'EOF'
GET / HTTP/1.1
Host: localhost:8080

EOF
```

サーバの応答を待つために 1 秒だけ待っています

## unix domain socket を叩く

/var/run/docker.sock も叩けます

```bash
#!/bin/bash
nc -U /var/run/docker.sock -q1 <<'EOF'
GET /containers/json?all=1&size=1 HTTP/1.1
Host: localhost:8080

EOF
```

デバッグなどに便利ですね

## 参考

* https://orebibou.com/2015/11/ncnetcat%E3%82%B3%E3%83%9E%E3%83%B3%E3%83%89%E3%81%A7%E8%A6%9A%E3%81%88%E3%81%A6%E3%81%8A%E3%81%8D%E3%81%9F%E3%81%84%E4%BD%BF%E3%81%84%E6%96%B98%E5%80%8B/
* http://blog.livedoor.jp/sonots/archives/34703829.html
* https://unix.stackexchange.com/questions/189454/netcat-doesnt-terminate-when-stdin-closes
* https://stackoverflow.com/questions/1270027/how-can-i-close-a-netcat-connection-after-a-certain-character-is-returned-in-the
