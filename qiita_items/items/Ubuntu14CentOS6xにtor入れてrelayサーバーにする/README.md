## centos

https://www.torproject.org/docs/rpms.html.en
ここを読みながら。

`/etc/yum.repos.d/torproject.repo`というファイルを作って以下の内容を書き込みます。

```/etc/yum.repos.d/torproject.repo
[tor]
name=Tor repo
enabled=1
baseurl=https://deb.torproject.org/torproject.org/rpm/el/6/$basearch/
gpgcheck=1
gpgkey=https://deb.torproject.org/torproject.org/rpm/RPM-GPG-KEY-torproject.org.asc
repo_gpgcheck=1

[tor-source]
name=Tor source repo
enabled=1
autorefresh=0
baseurl=https://deb.torproject.org/torproject.org/rpm/el/6/SRPMS
gpgcheck=1
gpgkey=https://deb.torproject.org/torproject.org/rpm/RPM-GPG-KEY-torproject.org.asc
repo_gpgcheck=1
```

`yum install tor`します。

## ubuntu

`apt-get install tor`します。

## 以後共通

`service tor start` します。

`/etc/tor/torrc` 以下の設定になるよう編集します。

```/etc/tor/torrc
Nickname hogehoge
ORPort 9001
BandwidthRate 20 KB
BandwidthBurst 50 KB
ExitPolicy reject *:*
```

その他 `iptables` などのファイアウォールを設定します。

`service tor restart` します。

https://atlas.torproject.org/
で 自分のNicknameを検索して動いているか確認します。

## 鍵ファイルのバックアップ
* https://www.torproject.org/docs/faq.html.en#UpgradeOrMove

`/var/lib/tor/keys/secret_id_key` 

リレーサーバーのIPなどを移動するときは鍵のバックアップを取っておきましょう

## ログの確認

* https://www.torproject.org/docs/faq.html.en#Logs

`cat /var/log/tor/log`


## 参考
* https://wiki.archlinuxjp.org/index.php/Tor
* https://www.torproject.org/
* https://www.torproject.org/docs/tor-doc-relay.html.en
* https://www.torproject.org/docs/tor-relay-debian.html.en
