__※ 追追記 [tokio_fs](https://github.com/tokio-rs/tokio/tree/master/tokio-fs) を使いましょう__
__※ 追記 [futures_fs](https://docs.rs/futures-fs/0.0.3/futures_fs) を使いましょう__

Rust で非同期 IO をするには tokio が便利である。
しかし tokio は非同期ネットワークライブラリであり、node.js のように非同期でファイルシステムを扱うことができない。
これは、[現状の Linux (Posix) には十分な非同期ファイルシステム用システムコールが整備されていないため](https://blog.libtorrent.org/2012/10/asynchronous-disk-io/)である。
実際、node.js の中で用いられている非同期IOライブラリである libuv も[ブロッキングなファイル操作はスレッドプールで待つことで非同期ファイル操作を実現している](http://docs.libuv.org/en/v1.x/threadpool.html#threadpool)。
それに留まらず、 [go](https://github.com/golang/go/issues/6817), [haskell(ghc)](https://ghc.haskell.org/trac/ghc/ticket/13296), [boost asio](https://lists.boost.org/Archives/boost/2011/03/178613.php) なども同様である。

そこで http ライブラリには [hyper](https://github.com/hyperium/hyper) を使い、スレッドプールには [futures_cpupool](https://github.com/rust-lang-nursery/futures-rs/tree/master/futures-cpupool) を使ったシンプルな静的ファイル配信Webサーバを実装した。


## 特徴
* 非同期ネットワーク操作用の `tokio_core::reactor::Handle` と、非同期ファイル操作用の `futures_cpupool::CpuPool` を状態として持つ
* ブロッキングな最低限のファイル操作を cpupool に投げる

## コード

```rust:main.rs
extern crate getopts;
extern crate pretty_env_logger;
#[macro_use] extern crate log;
extern crate tokio_core;
extern crate mio;
extern crate futures;
extern crate futures_cpupool;
extern crate hyper;
extern crate hyper_tls;

use std::fs::{self};
use std::vec::{Vec};
use std::boxed::{Box};
use std::result::Result::{Ok, Err};
use std::io::{self, BufReader, Read};
use std::path::{Path};
use std::time::{Duration};
use futures::{Future};
use futures::future;
use futures::stream::{Stream};
use futures_cpupool::{CpuPool};
use tokio_core::reactor::{Core, Timeout, Handle};
use hyper::{Body, Client, Get, Post, StatusCode, Chunk};
use hyper::server::{Http, Service, Request, Response};
use hyper::header::{ContentLength};

struct WebService {
  handle: Handle, // for async network task
  pool: CpuPool, // for async fileio task
  publicDir: String, // cwd
}

impl WebService {
  fn static_file(&self, path: String) -> Box<Future<Item=String, Error=String>> {
    let handle = self.handle.clone();
    let publicDir = self.publicDir.clone();
    let task = self.pool.spawn_fn(move ||{
      let path = match path.as_str() {
        "/" => "/index.html".to_string(),
        _ => path
      };
      let filepath = format!("{}{}", publicDir, path);
      println!("{}", filepath);
      match std::fs::File::open(filepath) {
        Err(why) => Err(why.to_string()),
        Ok(mut file) => {
          let mut s = String::new();
          match file.read_to_string(&mut s) {
            Err(why) => Err("cannot read to string".to_string()),
            Ok(_) => Ok(s)
          }
        }
      }
    });
    return Box::new(task);
  }
}

impl Service for WebService {
  type Request = Request;
  type Response = Response<Box<Stream<Item=Chunk, Error=Self::Error>>>;
  type Error = hyper::Error;
  type Future = Box<Future<Item=Self::Response, Error=Self::Error>>;
  fn call(&self, req: Self::Request) -> Self::Future {
    match (req.method(), req.path()) {
      (&Get, _) =>
        Box::new(
          self.static_file(req.path().to_string()).then(|res|{
            let (statusCode, bodyString) = match res {
              Ok(body) => (StatusCode::Ok, body),
              Err(_) => (StatusCode::NotFound, "Not Found".to_string()),
            };
            let length = bodyString.len() as u64;
            let body: Box<Stream<Item=Chunk, Error=Self::Error>> = Box::new(Body::from(bodyString));
            return future::ok(Response::new()
              .with_status(statusCode)
              .with_header(ContentLength(length))
              .with_body(body));
        })),
      _ => Box::new(future::ok(Response::new().with_status(StatusCode::InternalServerError))),
    }
  }
}

fn main() {
  pretty_env_logger::init();
  
  let args: Vec<String> = std::env::args().collect();
  let program = args[0].clone();
  let mut opts = getopts::Options::new();
  opts.optopt("p", "port", "port", "3000");
  opts.optflag("h", "help", "print this help menu");
  let matches = opts.parse(&args[1..]).unwrap();
  if matches.opt_present("h") {
    let help = format!("Usage: {} FILE [options]", program);
    print!("{}", opts.usage(&help));
    return;
  }

  let port = matches.opt_str("p").unwrap();
  let addr = format!("127.0.0.1:{}", port).parse().unwrap();

  // async io resource
  let pool = CpuPool::new_num_cpus(); // for file aio
  let mut core = Core::new().unwrap(); // for network aio
  
  // create web service
  let server_handle = core.handle();
  let service_pool = pool.clone();
  let service_handle = core.handle();
  let serve = Http::new().serve_addr_handle(&addr, &server_handle, move ||{
    let service = WebService{
      handle: service_handle.clone(),
      pool: service_pool.clone(),
      publicDir: std::env::current_dir().unwrap().to_str().unwrap().to_string(),
    };
    return Ok(service);
  }).unwrap();
  println!("Listening on http://{} with 1 thread.", serve.incoming_ref().local_addr());

  // launch web server
  let connection_handle = core.handle();
  server_handle.spawn(
    serve
      .for_each(move |conn| {
        // handle new connection
        connection_handle.spawn(
          conn
            .map(|_| ())
            .map_err(|err| println!("serve error: {:?}", err))
        );
        return Ok(());
      }).map_err(|_| ())
  );

  core.run(future::empty::<(), ()>()).unwrap();
}
```

```toml:Cargo.toml
[package]
name = "SimpleAIOWebServer"
version = "0.1.0"
authors = ["foo <foo@foo.com>"]

[dependencies]
futures = "0.1"
futures-cpupool = "0.1"
futures-await = "0.1"
log = "0.4"
pretty_env_logger = "0.2"
mio = "0.6"
tokio-core = "0.1"
hyper = "0.11"
hyper-tls = "0.1"
websocket = "0.20"
getopts = "0.2"
```

## 後記
* iron がいつまでたっても非同期 hyper に対応しないので hyper をそのまま使った
* 2018年1月現在、ネイティブの省リソース非同期ファイルサーバを作るなら C++14で書かれたWebサーバフレームワークの [crow](https://github.com/ipkn/crow) で [boost asio + boost filesystem + boost thread](https://stackoverflow.com/questions/11423426/how-does-libuv-compare-to-boost-asio) するのが楽そう
* nodejs の ReadStream がどうやって実装されているのか気になってきた
* [libuv の uv_stream_t](http://docs.libuv.org/en/v1.x/stream.html) を使っているようだ
* [uv__fs_read](https://github.com/libuv/libuv/blob/v1.x/src/unix/fs.c#L304) で [pread](https://linuxjm.osdn.jp/html/LDP_man-pages/man2/pread.2.html) を使っているらしい
* pread 逐次をスレッドプールの中で読んでいるように見える
* 「[RustのファイルI/OにはBufReader, BufWriterを使いましょう、という話](https://qiita.com/gyu-don/items/50f4239fc856bed00dc4) 」をスレッドプールで動かすような形になりそう
* [futures_fs](https://docs.rs/futures-fs/0.0.3/futures_fs) というそのものズバリなライブラリがあった

## futures-fs 版

せっかくなので futures-fs 版も作った。
↑の cpupool 版と違ってファイルを stream で読み書きしているのが特徴です。

今後の async iron やその他非同期フレームワークは tokio + hyper + futures-fs + ??? の組み合わせが多くなりそう

```rust:main.rs
extern crate getopts;
extern crate pretty_env_logger;
#[macro_use] extern crate log;
extern crate tokio_core;
extern crate mio;
extern crate futures;
extern crate futures_cpupool;
extern crate futures_fs;
extern crate bytes;
extern crate hyper;
extern crate hyper_tls;

use std::vec::{Vec};
use std::boxed::{Box};
use std::result::Result::{Ok, Err};
use std::path::{Path, PathBuf};
use std::time::{Duration};
use futures::{Future, Stream};
use futures::future;
use futures::stream;
use futures_cpupool::{CpuPool};
use futures_fs::FsPool;
use bytes::{Bytes};
use tokio_core::reactor::{Core, Timeout, Handle};
use hyper::{Body, Client, Get, Post, StatusCode, Chunk};
use hyper::server::{Http, Service, Request, Response};


struct WebService {
  handle: Handle, // for async network task
  fs: FsPool, // for async fileio stream task
  public_dir: PathBuf, // cwd
}

impl WebService {
  /// (PathBuf path) -> std::unique_ptr<std::fstream>
  fn static_file(&self, path: PathBuf) -> Result<Box<Stream<Item=Bytes, Error=std::io::Error>>, std::io::Error> {
    let path = if path == Path::new("/") { PathBuf::from("index.html") }else{ path };
    let path = if path.is_absolute() { PathBuf::from(path.to_str().unwrap()[1..].to_string()) }else{ path };
    // need cwd check here
    let filepath = self.public_dir.join(path);
    println!("{}", filepath.to_str().unwrap());
    match filepath.canonicalize() {
      Err(why) => Err(why),
      Ok(path) =>{
        let fin = self.fs.read(filepath);
        Ok(Box::new(fin)) // std::make_unique
      }
    }
  }
}

impl Service for WebService {
  type Request = Request;
  type Response = Response<Box<Stream<Item=Chunk, Error=Self::Error>>>;
  type Error = hyper::Error;
  type Future = Box<Future<Item=Self::Response, Error=Self::Error>>;
  fn call(&self, req: Self::Request) -> Self::Future {
    match (req.method(), req.path()) {
      (&Get, _) => {
        let body: Box<Stream<Item=Chunk, Error=Self::Error>> = match self.static_file(Path::new(req.path()).to_path_buf()){
          Ok(fin) => Box::new(fin.map(|byte| Chunk::from(byte)).map_err(|err| hyper::Error::Io(err))),
          Err(_) => Box::new(Body::from("Not Found".to_string())),
        };
        let res: Self::Response = Response::new().with_status(StatusCode::Ok).with_body(body);
        let fut = future::ok(res);
        Box::new(fut)
      },
      _ => {
        let body: Box<Stream<Item=Chunk, Error=Self::Error>> = Box::new(Body::from("Method Not Allowed".to_string()));
        let res: Self::Response = Response::new().with_status(StatusCode::MethodNotAllowed).with_body(body);
        let fut = future::ok(res);
        Box::new(fut)
      },
    }
  }
}

fn main() {
  pretty_env_logger::init();
  
  let args: Vec<String> = std::env::args().collect();
  let program = args[0].clone();
  let mut opts = getopts::Options::new();
  opts.optopt("p", "port", "port", "3000");
  opts.optflag("h", "help", "print this help menu");
  let matches = opts.parse(&args[1..]).unwrap();
  if matches.opt_present("h") {
    let help = format!("Usage: {} FILE [options]", program);
    print!("{}", opts.usage(&help));
    return;
  }

  let port = matches.opt_str("p").unwrap();
  let addr = format!("127.0.0.1:{}", port).parse().unwrap();

  // async io resource
  let fspool = FsPool::new(4);
  let mut core = Core::new().unwrap(); // for network aio
  
  // create web service
  let server_handle = core.handle();
  let service_handle = core.handle();
  let serve = Http::new().serve_addr_handle(&addr, &server_handle, move ||{
    let service = WebService{
      handle: service_handle.clone(),
      fs: fspool.clone(),
      public_dir: std::env::current_dir().unwrap().as_path().to_path_buf(),
    };
    return Ok(service);
  }).unwrap();
  println!("Listening on http://{} with 1 thread.", serve.incoming_ref().local_addr());

  // launch web server
  let connection_handle = core.handle();
  server_handle.spawn(
    serve
      .for_each(move |conn| {
        // handle new connection
        connection_handle.spawn(
          conn
            .map(|_| ())
            .map_err(|err| println!("serve error: {:?}", err))
        );
        return Ok(());
      }).map_err(|_| ())
  );

  core.run(future::empty::<(), ()>()).unwrap();
}
```


## 参考

* https://hyper.rs/guides/server/response_strategies/
* https://github.com/hyperium/hyper/blob/34f0dba6dcdd3c65da04fc277839d4fe129843f4/examples/web_api.rs

