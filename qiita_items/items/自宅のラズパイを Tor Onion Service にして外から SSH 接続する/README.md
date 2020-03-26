## 概要

* 家のラズパイに外から接続したい
* グローバルIPを持っていないので接続できない
* ラズパイの sshd を onion service にして世界中どこからでもアクセスできるようにする

はじめは SoftEther VPN on Azure しようとしたけどなんか繋がらなかったので Tor を使った。

## server (raspbian stretch) の設定

sshd の設定方法は省略。
グローバルIPを晒すときも同じだがセキュリティには十分配慮すること。

[例えば](http://makezine.jp/blog/2017/09/secure-your-raspberry-pi-against-attackers.html)

* SSHDはパスワードログインの無効化や公開鍵認証にする
* `uwf` などでファイヤフォールを設定
* `unattended-upgrades` などで自動アップデートする

など。

tor をインストール

```sh
sudo apt-get install tor
```

tor の onion service 設定で `/etc/tor/torrc` の 22 番を開けておく

```:/etc/tor/torrc
HiddenServiceDir /var/lib/tor/ssh_hidden_service/
HiddenServicePort 22 127.0.0.1:22
```

デーモンを再起動

```sh
sudo systemctl restart tor
```

Onion Service の hostname をメモっておく

```sh
sudo cat /var/lib/tor/ssh_hidden_service/hostname 
```

## client (Ubuntu 18.04 Bionic Beaver) の設定

```
sudo apt-get install tor connect-proxy
```

プロクシを設定する。
プロクシのやり方は `connect-proxy` を使う以外にも色々あるようだ（下記リンク参照）

```
cat <<'EOF'>> ~/.ssh/config
Host *.onion
        ProxyCommand connect -R remote -5 -S 127.0.0.1:9050 %h %p
        ForwardX11 yes
EOF
```

接続（鍵設定は省略）

```
ssh user@hostname.onion
```

onion service の hostname は公の場には公開しないこと。

## [おまけ] Termux (0.60) で Android から接続する


```sh
apt install tor torsocks openssh
```

termux には systemd がないので自分でデーモン起動する

```sh
tor&
```

接続（鍵設定は省略）

```sh
ssh user@hostname.onion
```

## [おまけ] raspberry pi に最新版の Tor を入れて Onion Service v3 を使う

stretch に入ってる `tor` は 0.2x 系なので、最新の 0.3x が欲しい場合

https://www.torproject.org/docs/debian.html.en

を参考に 0.3 系のリポジトリを登録する必要があります。


```sh
cat <<'EOF' | sudo tee -a /etc/apt/sources.list
deb https://deb.torproject.org/torproject.org stretch main
#deb-src https://deb.torproject.org/torproject.org stretch main
deb https://deb.torproject.org/torproject.org tor-experimental-0.3.3.x-stretch main
#deb-src https://deb.torproject.org/torproject.org tor-experimental-0.3.3.x-stretch main
EOF
```

この `tor-experimental-0.3.3.x-stretch` は 2018 年 6 月現在のものです。上記 torproject のサイトでより新しいバージョンがないか確認してください。


```sh
sudo apt-get install dirmngr --install-recommends
gpg --keyserver keys.gnupg.net --recv A3C4F0F979CAA22CDBA8F512EE8CBC9E886DDD89
gpg --export A3C4F0F979CAA22CDBA8F512EE8CBC9E886DDD89 | sudo apt-key add -
```

```sh
apt update
apt install tor deb.torproject.org-keyring
```

これで 0.3 系が使えるようになります。

tor 0.3x からは より秘匿性が高く安全な Onion Service v3 が使えるようになります。設定は下記 URL を参考にしてください。

* https://www.jamieweb.net/blog/onionv3-hidden-service/
* https://www.deepdotweb.com/2017/10/14/tor-update-supports-v3-onion-services/

## 感想

* グローバル IP は不便
* Onion Service 便利
* Termux すごい


## 参考


### raspberry pi セキュリティ設定
* http://makezine.jp/blog/2017/09/secure-your-raspberry-pi-against-attackers.html
* https://blog.dantup.com/2016/04/setting-up-automatic-updates-on-raspberry-pi-raspbian-jessie/
* https://qiita.com/Fendo181/items/659f306232f55fc5a8de
* https://wiki.debian.org/UnattendedUpgrades
* https://www.ohsan.info/2017/12/raspistretch_15.html


### SSH and Tor

* https://www.khalidalnajjar.com/access-your-raspberry-pi-globally-using-tor/
* https://gk2.sk/running-ssh-on-a-raspberry-pi-as-a-hidden-service-with-tor/
* https://tor.stackexchange.com/questions/7064/how-to-ssh-to-a-hidden-service-with-torsocks

## Tor and the Internet of Things 
* https://blog.torproject.org/quick-simple-guide-tor-and-internet-things-so-far
