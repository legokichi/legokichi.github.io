
[dockerd の API は docker.sock を介して REST API になってます。デフォルトは unix domain socket になっていて、起動オプションで http に変えることができます。](https://docs.docker.com/engine/reference/commandline/dockerd/#examples)

[tokio-uds](https://github.com/tokio-rs/tokio/tree/master/tokio-uds) を使って unix domain socket 経由で [docker engine api](https://docs.docker.com/engine/api/v1.24/) を叩いてみましょう。

```toml
[dependencies]
futures = "0.1"
mdo = "0.3"
mdo-future = "0.2"
tokio = "0.1"
tokio-uds = "0.2"
tokio-timer = "0.2"
```

```rust
extern crate futures;
extern crate tokio;
extern crate tokio_uds;
extern crate tokio_timer;
#[macro_use]
extern crate mdo;
extern crate mdo_future;

use mdo_future::future::*;
use futures::prelude::*;
use futures::future;
use tokio_uds::UnixStream;

fn main() {
    let docker_sock = std::path::Path::new("/var/run/docker.sock");
    tokio::run(mdo!{
        mut client =<< UnixStream::connect(&docker_sock);
        _ =<< {
            use std::io::Write;
            let request = format!("GET /containers/json?all=1&size=1 HTTP/1.1\r\nHOST: rust\r\n\r\n");
            println!("{}", request);
            future::result(client.write_all(request.as_bytes()))
        };
        _ =<< tokio_timer::sleep(std::time::Duration::from_secs(1)).map_err(|err| std::io::Error::new(std::io::ErrorKind::Other, format!("{:?}", err)));
        _ =<< {
            use std::io::Read;
            const BUFFER_SIZE: usize = 1024;
            let mut buffer: [u8; BUFFER_SIZE] = [0; BUFFER_SIZE];
            let mut raw: Vec<u8> = Vec::new();
            loop {
                let ret = client.read(&mut buffer).map(|len|{
                    println!("{}", len);
                    for i in 0..len { raw.push(buffer[i]); }
                });
                if let Err(err) = ret {
                    if err.kind() == std::io::ErrorKind::WouldBlock {
                        break;
                    }
                    panic!("{:?}", err);
                }
            }
            let ret = std::str::from_utf8(&raw).unwrap();
            println!("{}", ret);
            future::ok(())
        };
        ret future::ok(())
    }.map_err(|_| () ));
}

```

これを hyper を使って実装したものが [Boondock](https://github.com/faradayio/boondock) です。


