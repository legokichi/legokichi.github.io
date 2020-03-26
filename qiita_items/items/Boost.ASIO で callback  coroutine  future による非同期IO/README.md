この記事はきっと [C++ Advent Calendar 2017](https://qiita.com/advent-calendar/2017/cpp) の 12/13 の記事です。

# Boost.ASIO で callback | coroutine | future による非同期IO

とっちらかった記事です。表題の他に最近 Boost に入った Boost.Beast や Boost.Fiber と ASIO の関係についても少し書きます。

## Boost.ASIO + Boost.Coroutine について
`boost::asio::spawn` を使うと非同期IOにありがちなコールバック地獄を避けて書けるようになります。
C#、　F#、 TypeScript などの async のようなものです。

以下のデモコードは JS の setTimeout のようにスレッドをスリープさせることなく非同期コールバックタイマで非同期逐次処理を行っています。5秒おきに数字が一文字出力されます。

```c++
#include <iostream>
#include <boost/asio.hpp>
#include <boost/asio/spawn.hpp>

int main() {
  using boost::asio::io_service;
  using boost::asio::deadline_timer;
  namespace asio = boost::asio;
  namespace ptime = boost::posix_time;
  auto const ios = std::make_shared<io_service>();
  asio::spawn(*ios, [ios](auto yield) {
    std::cout << "0" << std::endl;
    auto timer1 = deadline_timer{*ios};
    timer1.expires_from_now(ptime::seconds(5));
    timer1.async_wait(yield);
    std::cout << "1" << std::endl;
    auto timer2 = deadline_timer{*ios};
    timer2.expires_from_now(ptime::seconds(5));
    timer2.async_wait(yield);
    std::cout << "2" << std::endl;
  });
  ios->run();
  return EXIT_SUCCESS;
}
```

上の async_wait のラッパ関数 wait を作ってみます。

```c++
#include <iostream>
#include <utility>
#include <boost/asio.hpp>
#include <boost/asio/spawn.hpp>
using std::shared_ptr;
namespace asio = boost::asio;
using boost::system::error_code;

template<class CompletionToken, class ReturnType>
using HandlerType = typename asio::handler_type<CompletionToken, void(ReturnType)>::type;

template<class CompletionToken, class ReturnType>
using AsyncResult = typename asio::async_result<HandlerType<CompletionToken, ReturnType>>::type;

template<class CompletionToken>
auto wait(
  const shared_ptr<asio::io_service> ios,
  const boost::posix_time::time_duration time,
  CompletionToken&& token
)-> AsyncResult<CompletionToken, error_code> {
  using handler_t = HandlerType<CompletionToken, error_code>;
  auto handler = handler_t{std::forward<CompletionToken>(token)};
  auto result = asio::async_result<handler_t>{handler};
  asio::spawn(*ios, [=](auto yield) mutable {
    auto ec = error_code{};
    auto timer = asio::deadline_timer{*ios};
    timer.expires_from_now(time);
    timer.async_wait(yield[ec]);
    handler(std::move(ec));
  });
  return result.get();
}
```

こう型定義しておくと、この非同期 wait 関数は callback形式、coroutine形式、future形式のいずれの記法でも使えるようになります。

### callback 形式

コールバックネストが発生しています。たくさんの非同期処理を書くのは大変そうです。

```c++
int main() {
  using boost::asio::io_service;
  using boost::asio::deadline_timer;
  namespace asio = boost::asio;
  namespace ptime = boost::posix_time;
  auto const ios = std::make_shared<io_service>();
  std::cout << "0" << std::endl;
  wait(ios, ptime::milliseconds(5000), [=](auto ec){
    std::cout << "1" << std::endl;
    wait(ios, ptime::milliseconds(5000), [=](auto ec){
      std::cout << "2" << std::endl;
    });
  });
  ios->run();
  return EXIT_SUCCESS;
}
```

### coroutine 形式

Boost.Coroutine を使って擬似的にコルーチンを実現しています。

```c++
int main() {
  using boost::asio::io_service;
  using boost::asio::deadline_timer;
  namespace asio = boost::asio;
  namespace ptime = boost::posix_time;
  auto const ios = std::make_shared<io_service>();
  asio::spawn(*ios, [ios](auto yield) {
    auto ec = boost::system::error_code{};
    std::cout << "0" << std::endl;
    wait(ios, ptime::milliseconds(5000), yield[ec]);
    std::cout << "1" << std::endl;
    wait(ios, ptime::milliseconds(5000), yield[ec]);
    std::cout << "2" << std::endl;
  });
  ios->run();
  return EXIT_SUCCESS;
}
```

### future 形式

C++14 で入った Promise-Future パターンを使うことができます。
ただしFutureパターンはFutureの返す値をgetしたときに待受が発生し同期処理になるため、イベントループはgetするスレッドとは別のスレッドで動かしておく必要があります。

```c++
#include <thread>
#include <boost/asio/use_future.hpp>

int main() {
  using boost::asio::io_service;
  using boost::asio::deadline_timer;
  namespace asio = boost::asio;
  namespace ptime = boost::posix_time;
  auto const ios = std::make_shared<io_service>();
  auto io_thread = std::thread{[=]() { ios->run(); }};
  auto work = asio::io_service::work{*ios};
  try {
    std::cout << "0" << std::endl;
    wait(ios, ptime::milliseconds(5000), asio::use_future).get();
    std::cout << "1" << std::endl;
    wait(ios, ptime::milliseconds(5000), asio::use_future).get();
    std::cout << "2" << std::endl;
  }catch (boost::system::system_error error){
    // ...
  }
  ios->stop();
  io_thread.join();
  return EXIT_SUCCESS;
}
```

### うまくいくわけ

このテクニックのキモは `CompletionToken&& token` という右辺値参照を `std::forward<CompletionToken>(token)` で `boost::asio::handler_type` コンストラクタの引数としての右辺値にキャストしているところです。 `boost::asio::handler_type` コンストラクタは引数の型によって挙動が変わります。

* 特殊化なし。コールバックで使用 - https://github.com/boostorg/asio/blob/9bfe806fc1d2a51c97b433571f42b83606a28b05/include/boost/asio/handler_type.hpp#L34
* futureで特殊化 - https://github.com/boostorg/asio/blob/9bfe806fc1d2a51c97b433571f42b83606a28b05/include/boost/asio/impl/use_future.hpp
* spawn の yield で特殊化 - https://github.com/boostorg/asio/blob/9bfe806fc1d2a51c97b433571f42b83606a28b05/include/boost/asio/impl/spawn.hpp

## Boost.Beast で使ってみる

Boost 1.66.0 から新しいライブラリ Boost Beast が入りました。
Boost.ASIO 上に構築されたネットワークライブラリです。
HTTP, WebSocket などを Boost.ASIO よりも楽に使うことができます。
cpp-netlib や websocketpp を統合したような立ち位置のようです。
現時点では URL パーサがライブラリについていませんが、近いうちに追加されるようです。

### HTTP | HTTPS クライアント

```c++
#include <cstdlib>
#include <string>
#include <memory>
#include <optional>
#include <variant>
#include <utility>
#include <tuple>
#include <boost/asio.hpp>
#include <boost/asio/spawn.hpp>
#include <boost/asio/ssl.hpp>
#include <boost/asio/connect.hpp>
#include <boost/asio/ip/tcp.hpp>
#include <boost/asio/ssl/stream.hpp>
#include <boost/beast/core.hpp>
#include <boost/beast/version.hpp>
#include <boost/beast/http.hpp>
#include <boost/beast/websocket.hpp>

using std::string;
using std::pair;
using std::tuple;
using std::variant;
using std::optional;
using std::shared_ptr;

namespace asio = boost::asio;
namespace beast = boost::beast;
using boost::asio::ip::tcp;
namespace ssl = boost::asio::ssl;
namespace http = boost::beast::http;
using boost::system::error_code;

template<class CompletionToken, class ReturnType>
using HandlerType = typename asio::handler_type<CompletionToken, void(ReturnType)>::type;

template<class CompletionToken, class ReturnType>
using AsyncResult = typename asio::async_result<HandlerType<CompletionToken, ReturnType>>::type;

template<class CompletionToken>
auto httpRequest(
  const shared_ptr<asio::io_service> ios,
  const string host,
  const http::request<http::string_body> req,
  CompletionToken&& token
)-> AsyncResult<CompletionToken, variant<string, http::response<http::string_body>>> {
  using ret_t = variant<string, http::response<http::string_body>>;
  using handler_t = HandlerType<CompletionToken, ret_t>;
  auto handler = handler_t{std::forward<CompletionToken>(token)};
  auto result = asio::async_result<handler_t>{handler};
  asio::spawn(*ios, [=](auto yield) mutable {
    auto ec = error_code{};
    auto query  = tcp::resolver::query{host, "http"};
    auto lookup = tcp::resolver{*ios}.async_resolve(query, yield[ec]);
    if(ec != 0){ return handler(ec, ret_t{"lookup error"}); }
    auto socket = tcp::socket{*ios};
    asio::async_connect(socket, lookup, yield[ec]);
    if(ec != 0){ return handler(ret_t{"connect error"}); }
    http::async_write(socket, const_cast<http::request<http::string_body>&>(req), yield[ec]);
    if(ec != 0){ return handler(ret_t{"write error"}); }
    auto buffer = beast::flat_buffer{};
    auto res = http::response<http::string_body>{};
    http::async_read(socket, buffer, res, yield[ec]);
    if(ec != 0){ return handler(ret_t{"read error"}); }
    socket.shutdown(tcp::socket::shutdown_both, ec);
    if(ec != 0){ return handler(ret_t{"shutdown error"}); }
    handler(ret_t{res});
  });
  return result.get();
}

template<class CompletionToken>
auto httpsRequest(
  const shared_ptr<asio::io_service> ios,
  const string host,
  const http::request<http::string_body> req,
  CompletionToken&& token
)-> AsyncResult<CompletionToken, variant<string, http::response<http::string_body>>> {
  using ret_t = variant<string, http::response<http::string_body>>;
  using handler_t = HandlerType<CompletionToken, ret_t>;
  auto handler = handler_t{std::forward<CompletionToken>(token)};
  auto result = asio::async_result<handler_t>{handler};
  asio::spawn(*ios, [=](auto yield) mutable {
    auto ec = error_code{};

    auto query = tcp::resolver::query{host, "https"};
    auto lookup = tcp::resolver{*ios}.async_resolve(query, yield[ec]);
    std::cout << "dns lookup:" << ec << std::endl;
    if(ec != 0){ return handler(ec, ret_t{"lookup error"}); }

    auto ctx = ssl::context{ssl::context::sslv23};
    auto ssl_socket = ssl::stream<tcp::socket>{*ios, ctx};
    asio::async_connect(ssl_socket.lowest_layer(), lookup, yield[ec]);
    std::cout << "tcp connect:" << ec << std::endl;
    if(ec != 0){ return handler(ec, ret_t{"connection error"}); }

    ssl_socket.async_handshake(ssl::stream_base::client, yield[ec]);
    std::cout << "ssl handshake:" << ec << std::endl;
    if(ec != 0){ return handler(ec, ret_t{"handshake error"}); }

    http::async_write(ssl_socket, const_cast<http::request<http::string_body>&>(req), yield[ec]);
    std::cout << "http write:" << ec << std::endl;

    auto res = http::response<http::string_body>{};
    auto buffer = beast::flat_buffer{};
    http::async_read(ssl_socket, buffer, res, yield[ec]);
    std::cout << "http read:" << ec << std::endl;

    ssl_socket.lowest_layer().cancel(ec);
    std::cout << "tcp cancel:" << ec << std::endl;

    ssl_socket.async_shutdown(yield[ec]);
    std::cout << "ssl shutdown:" << ec << std::endl;

    ssl_socket.lowest_layer().shutdown(tcp::socket::shutdown_both, ec);
    std::cout << "tcp shutdown:" << ec << std::endl;

    ssl_socket.lowest_layer().close(ec);
    std::cout << "tcp close:" << ec << std::endl;

    return handler(ret_t{res});
  });
  return result.get();
}

auto main(int argc, char* argv[])-> int {
  auto ios = std::make_shared<boost::asio::io_service>();
  boost::asio::spawn(*ios, [=](auto yield) mutable {
    auto ec = boost::system::error_code{};
    auto host = "google.com";
    auto req = http::request<http::string_body>{http::verb::get, "/", 11};
    {
      auto ret = httpRequest(ios, host, req, yield[ec]);
      if(auto res_ptr = std::get_if<http::response<http::string_body>>(&ret)){
        std::cout << *res_ptr << std::endl;
      }else if(auto err_ptr = std::get_if<std::string>(&ret)){
        std::cout << *err_ptr << std::endl;
      }
    }
    {
      auto ret = httpsRequest(ios, host, req, yield[ec]);
      if(auto res_ptr = std::get_if<http::response<http::string_body>>(&ret)){
        std::cout << *res_ptr << std::endl;
      }else if(auto err_ptr = std::get_if<std::string>(&ret)){
        std::cout << *err_ptr << std::endl;
      }
    }
  });
  ios->run();
  std::cout << "end" << std::endl;
  return EXIT_SUCCESS;
}
```

HTTP を書きやすくなったとはいえ、 HTTP GET をひとつ投げるのも一苦労ですね。

### WS | WSS クライアント

Boost.Beast の目玉機能はこっちなのですが、自前でサンプルコードを用意する時間がとれなかったので以下のリンクを読んでください。

* http://www.boost.org/doc/libs/develop/libs/beast/doc/html/beast/quick_start.html 
* https://github.com/boostorg/beast/blob/develop/example/websocket/client/async-ssl/websocket_client_async_ssl.cpp

## Boost.Fiber
Boost としては非推奨となった Boost.Coroutine を使った実装よりも Boost.Context によるコンテキストスイッチを使った Boost.Coroutine2 、そしてその上位互換である Boost.Fiber という goroutine ライクの軽量スレッドを押しているようです。
しかし既存の ASIO のスケジューラと Fiber のスケジューラがそれぞれイベントループを持っているため、Boost.Fiber と ASIO を統合するのは一筋縄ではいかないようです。

* 
Then There’s Boost.Asio -  http://www.boost.org/doc/libs/develop/libs/fiber/doc/html/fiber/callbacks/then_there_s____boost_asio__.html
* Deeper Dive into Boost.Asio -
 http://www.boost.org/doc/libs/develop/libs/fiber/doc/html/fiber/integration/deeper_dive_into___boost_asio__.html

## 参考
* Boost 1.66.0 のアナウンス - http://www.boost.org/users/history/version_1_66_0.html
* Boost Beast のドキュメント http://www.boost.org/doc/libs/develop/libs/beast/doc/html/
* Boost Beast のシンボル一覧 - http://www.boost.org/doc/libs/develop/libs/beast/doc/html/beast/
* Boost ASIO のシンボル一覧 - http://www.boost.org/doc/libs/develop/doc/html/boost_asio/reference.html
* Boost Beast のリポジトリ - https://github.com/boostorg/beast
* LAST CALL FOR CHANGES BEFORE BOOST 1.66.0 !!!! - https://github.com/boostorg/beast/issues/812
* [Feature Request] URI parser - https://github.com/boostorg/beast/issues/787
* Future of Beast  - https://github.com/boostorg/beast/issues/343
* callback, future, coroutine のいずれの形式でも呼び出せる asio 関数のレシピ -  https://gist.github.com/inetic/dc9081baf45ec4b60037
* CompletionToken について - https://stackoverflow.com/questions/24497881/boostasiospawn-yield-as-callback
* CompletionToken について(pdf) - http://www.open-std.org/Jtc1/sc22/wg21/docs/papers/2014/n4045.pdf
* Boost.Fiber - http://www.boost.org/doc/libs/develop/libs/fiber/doc/html/index.html
* Boost.Fiber のリポジトリ - https://github.com/boostorg/fiber/


## 付録

### 最新の boost ビルド方法

```sh
git clone --recursive --depth 1 https://github.com/boostorg/boost.git
cd boost
./bootstrap.sh
./b2 headers
./b2 install -j4 --prefix=../../local
```
