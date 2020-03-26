# Rust で学ぶ epoll

## 動機
* Rust の futures はゼロコスト抽象化を謳っており、 Future トレイトは poll メソッドを備えている
* tokio の polling 戦略を知るには epoll の知識が避けて通れない - https://tokio.rs/docs/getting-started/tasks/ , https://cafbit.com/post/tokio_internals/ , 
* epoll の例は C はたくさんあるけど Rust + nix は皆無だったのでやってみた

## コード

```rust
extern crate nix;
use nix::sys::epoll::*;
use nix::sys::socket::*;
use nix::unistd::close;
use std::collections::HashMap;
use std::os::unix::io::RawFd;

/// TCP connection
/// Read -> Write -> End
#[derive(Hash, Eq, PartialEq, Debug, Clone, Copy)]
enum State {
    Read,
    Write,
}

fn a()-> nix::Result<()> {
    let epfd = epoll_create()?;

    // epoll_event を入れる バッファ()
    // tokio では 1024 - https://github.com/tokio-rs/tokio/blob/c25ea78ec93f0eaa35bed3b61c7e98a408784a53/tokio-reactor/src/lib.rs#L248
    let mut epoll_events = vec![EpollEvent::empty(); 1024];
    let mut clients: HashMap<RawFd, State> = HashMap::new();
    
    // 接続待ち socket の fd
    let sockfd = socket(AddressFamily::Inet, SockType::Stream, SockFlag::SOCK_CLOEXEC, SockProtocol::Tcp)?;
    let addr = SockAddr::new_inet(InetAddr::new(IpAddr::new_v4(127, 0, 0, 1), 8080));
    println!("server fd: {}", sockfd);

    // local address に bind
    bind(sockfd, &addr)?;
    // backlog引数 が 1024 の理由 - https://github.com/carllerche/mio/pull/623
    listen(sockfd, 1024)?;

    // おそらく EpollEvent::new の引数の型が u64 なのは RawFd = i32 が -1 のときはエラーだから
    let mut ev = EpollEvent::new(EpollFlags::EPOLLIN, sockfd as u64);
    epoll_ctl(epfd, EpollOp::EpollCtlAdd, sockfd, &mut ev)?;
    
    loop{
        // 非同期 IO イベントが発生するまでスレッドをブロック
        //  -1 はタイムアウトなし ( 無限に待つ )
        let nfds = epoll_wait(epfd, &mut epoll_events, -1)?;
        // n 個の file descripter に何らかのイベントが発生した
        println!("epoll_wait: nfds={}", nfds);

        for i in 0..nfds {
            let data = epoll_events[i].data();
            let events = epoll_events[i].events();
            println!("i: {}, fd: {:?}, events: {:?}", i, data, events);
            let fd =  data as i32;
            // data にはイベントが発生した file descripter が入っている
            // events はそのイベントの状態がビットで格納されてる

            // 待受 socket が読み込み可能になった
            if fd == sockfd && events == events & EpollFlags::EPOLLIN {
                // socket への接続を accept して client と通信するための file descripter を作成
                let client_fd = accept(sockfd)?;
                println!("  accept client fd: {:?}", client_fd);

                // client_fd を epoll の監視対象に入れて(EpollCtlAdd)
                // read 可能になるのを待つ (client が http requeset を送信してくるのを待つ)
                let mut ev = EpollEvent::new(EpollFlags::EPOLLIN | EpollFlags::EPOLLONESHOT, client_fd as u64);
                epoll_ctl(epfd, EpollOp::EpollCtlAdd, client_fd, &mut ev)?;

                clients.insert(client_fd, State::Read);
                continue;
            }
            // accept 済の client からの epoll event 
            if clients.contains_key(&fd) {
                let client_fd = fd;
                let state = *clients.get(&client_fd).unwrap();
                println!("  client_fd: {:?}, state: {:?}, events: {:?}", client_fd, state, events);

                if events == events & EpollFlags::EPOLLIN && state == State::Read {
                    loop{
                        let mut buf: [u8; 64] = [0; 64];
                        let size = recv(client_fd, &mut buf, MsgFlags::empty())?;
                        let req = std::str::from_utf8(&buf).unwrap().to_string();
                        println!("    recv: buf: {:?}, size: {}", req, size);

                        // http request が終わるまで read し続ける
                        if !( req.find("\n\n").is_some() || req.find("\r\n\r\n").is_some() ){
                            continue;
                        }

                        // http request が終わった
                        // epoll の監視対象の client_fd を 
                        // write 可能になるのを待つように変更(EpollCtlMod)
                        let mut ev = EpollEvent::new(EpollFlags::EPOLLOUT, client_fd as u64);
                        epoll_ctl(epfd, EpollOp::EpollCtlMod, client_fd, &mut ev)?;

                        clients.insert(client_fd as i32, State::Write);
                        break;
                    }
                }else if events == events & EpollFlags::EPOLLOUT && state == State::Write {
                    // keep-aive 要求が来ても無視して Connection: close
                    let buf = "HTTP/1.1 200 Ok\nConnection: close\nContent-Type: text/plain\n\nha?\n\n";
                    let size = send(client_fd, buf.as_bytes(), MsgFlags::empty())?;
                    println!("    send: buf: {:?}, size: {}", buf, size);

                    // client_fd を epoll の監視対象から外す
                    epoll_ctl(epfd, EpollOp::EpollCtlDel, client_fd, &mut epoll_events[i])?;

                    // 接続中 client 一覧から削除
                    clients.remove(&client_fd);

                    // tcp 切断
                    shutdown(client_fd, Shutdown::Both)?;
                    close(client_fd)?;
                }
                continue;
            }
        }
    }
}

fn main() {
    println!("{:?}", a());
}
```

出力

```console
$ cargo run
server fd: 4
epoll_wait: nfds=1
i: 0, fd: 4, events: EPOLLIN
  accept client fd: 5
epoll_wait: nfds=1
i: 0, fd: 5, events: EPOLLIN
  client_fd: 5, state: Read, events: EPOLLIN
    recv: buf: "GET / HTTP/1.1\r\nHost: localhost:8080\r\nConnection: keep-alive\r\nPr", size: 64
    recv: buf: "agma: no-cache\r\nCache-Control: no-cache\r\nUpgrade-Insecure-Reques", size: 64
    recv: buf: "ts: 1\r\nDNT: 1\r\nUser-Agent: Mozilla/5.0 (X11; Linux x86_64) Apple", size: 64
    recv: buf: "WebKit/537.36 (KHTML, like Gecko) Chrome/67.0.3396.87 Safari/537", size: 64
    recv: buf: ".36\r\nAccept: text/html,application/xhtml+xml,application/xml;q=0", size: 64
    recv: buf: ".9,image/webp,image/apng,*/*;q=0.8\r\nAccept-Encoding: gzip, defla", size: 64
    recv: buf: "te, br\r\nAccept-Language: ja,en;q=0.9\r\n\r\n\u{0}\u{0}\u{0}\u{0}\u{0}\u{0}\u{0}\u{0}\u{0}\u{0}\u{0}\u{0}\u{0}\u{0}\u{0}\u{0}\u{0}\u{0}\u{0}\u{0}\u{0}\u{0}\u{0}\u{0}", size: 40
epoll_wait: nfds=1
i: 0, fd: 5, events: EPOLLOUT
  client_fd: 5, state: Write, events: EPOLLOUT
    send: buf: "HTTP/1.1 200 Ok\nConnection: close\nContent-Type: text/plain\n\nha?\n\n", size: 65
epoll_wait: nfds=1
i: 0, fd: 4, events: EPOLLIN
  accept client fd: 5
epoll_wait: nfds=1
i: 0, fd: 5, events: EPOLLIN
  client_fd: 5, state: Read, events: EPOLLIN
    recv: buf: "GET /favicon.ico HTTP/1.1\r\nHost: localhost:8080\r\nConnection: kee", size: 64
    recv: buf: "p-alive\r\nPragma: no-cache\r\nCache-Control: no-cache\r\nDNT: 1\r\nUser", size: 64
    recv: buf: "-Agent: Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTM", size: 64
    recv: buf: "L, like Gecko) Chrome/67.0.3396.87 Safari/537.36\r\nAccept: image/", size: 64
    recv: buf: "webp,image/apng,image/*,*/*;q=0.8\r\nReferer: http://localhost:808", size: 64
    recv: buf: "0/\r\nAccept-Encoding: gzip, deflate, br\r\nAccept-Language: ja,en;q", size: 64
    recv: buf: "=0.9\r\n\r\n\u{0}\u{0}\u{0}\u{0}\u{0}\u{0}\u{0}\u{0}\u{0}\u{0}\u{0}\u{0}\u{0}\u{0}\u{0}\u{0}\u{0}\u{0}\u{0}\u{0}\u{0}\u{0}\u{0}\u{0}\u{0}\u{0}\u{0}\u{0}\u{0}\u{0}\u{0}\u{0}\u{0}\u{0}\u{0}\u{0}\u{0}\u{0}\u{0}\u{0}\u{0}\u{0}\u{0}\u{0}\u{0}\u{0}\u{0}\u{0}\u{0}\u{0}\u{0}\u{0}\u{0}\u{0}\u{0}\u{0}", size: 8
epoll_wait: nfds=1
i: 0, fd: 5, events: EPOLLOUT
  client_fd: 5, state: Write, events: EPOLLOUT
    send: buf: "HTTP/1.1 200 Ok\nConnection: close\nContent-Type: text/plain\n\nha?\n\n", size: 65

```

## 感想

* Rust + nix のコードは C の epoll の例よりわかりやすい
* でも nix の ドキュメントは system call の説明がないので結局 C のサンプルコードを参考にするしかなかった
* nix にも mio にも timerfd がなかった(たぶん linux 専用だから - https://github.com/carllerche/mio/issues/360
* mio は libc 直接叩いてた
* rust の非同期まわりが複雑怪奇
    * futures トレイトは libcore に入った
    * futures は rust-lang-nursery にいる - https://github.com/rust-lang-nursery/futures-rs
    * でも futures の設計には mio と tokio が深く関わっている
    * ~~すべての黒幕は Aturon~~ 
* mio や tokio が出た当時の 2016 年代の古文書を漁るしかなさそう


## 参考

* https://docs.rs/nix/
* https://github.com/nix-rust/nix
* https://github.com/carllerche/mio
* https://linuxjm.osdn.jp/html/LDP_man-pages/man2/listen.2.html
* https://linuxjm.osdn.jp/html/LDP_man-pages/man7/epoll.7.html
* https://linuxjm.osdn.jp/html/LDP_man-pages/man2/epoll_create.2.html
* https://linuxjm.osdn.jp/html/LDP_man-pages/man2/epoll_ctl.2.html
* https://linuxjm.osdn.jp/html/LDP_man-pages/man2/epoll_wait.2.html
* https://linuxjm.osdn.jp/html/LDP_man-pages/man2/recv.2.html
* https://linuxjm.osdn.jp/html/LDP_man-pages/man2/send.2.html
* Server and client example with C sockets on Linux - https://www.binarytides.com/server-client-example-c-sockets-linux/
* Socket を利用した簡単な HTTP クライアントの実装 - https://python.keicode.com/advanced/socket.php
* 10-3 epoll による並列処理 - http://www.geekpage.jp/programming/linux-network/book/10/10-3.php
* ソケット通信 - http://www.ne.jp/asahi/hishidama/home/tech/socket/

## バージョン情報
* rustc 1.26.2
* nix 0.11.0



