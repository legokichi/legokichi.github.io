この記事はおそらく [https://qiita.com/advent-calendar/2017/opencv](OpenCV Advent Calendar 2017) の 12/12 の記事です。

# OpenCV で並列パイプライン処理(mutex, boost::lockfree, tbb::pipeline)の比較

## 動機
* VideoCapture -> 物体追跡とか重い処理 -> VideoWriter な処理がしたい
* パイプライン並列にできそう

```cpp
int main_naive(string INPUT_VIDEO_PATH, string OUTPUT_VIDEO_PATH){
  auto reader = cv::VideoCapture{ INPUT_VIDEO_PATH };
  if(!reader.isOpened()){
    printf("C++: reader cannot opened\n");
    return EXIT_FAILURE;
  }
  int width = reader.get(CV_CAP_PROP_FRAME_WIDTH);
  int height = reader.get(CV_CAP_PROP_FRAME_HEIGHT);
  double fps = reader.get(CV_CAP_PROP_FPS);
  auto writer = cv::VideoWriter{ "appsrc ! videoconvert ! x264enc ! mp4mux ! filesink location=" + OUTPUT_VIDEO_PATH + "  ", 0, fps, cv::Size{ width, height }, true};
  cv::Mat src;
  cv::Mat mid1, mid2;
  cv::Mat dst;
  while(reader.read(src)){
    cv::cvtColor(src, mid1, cv::COLOR_BGR2GRAY);
    cv::threshold(mid1, mid2, 0, 255, cv::THRESH_BINARY);
    std::this_thread::sleep_for(std::chrono::milliseconds(10)); // なんか重い処理
    cv::cvtColor(mid2, dst, cv::COLOR_GRAY2BGR);
    writer.write(dst);
  }
  return EXIT_SUCCESS;
}
```

## パイプライン並列を作る
### mutex と queue を使う
* 古来からの技法
* `lock_guard` で Scoped Locking Pattern を使った

```cpp
int main_mutex(cv::VideoCapture& reader, cv::VideoWriter& writer){
  using std::thread;
  using std::mutex;
  using std::lock_guard;
  using std::queue;
  mutex src_que_mutex;
  queue<cv::Mat*> src_que;
  mutex dst_que_mutex;
  queue<cv::Mat*> dst_que;
  auto source_th = thread{[&](){
    while(true){
      auto src = new cv::Mat{};
      if(reader.read(*src)){
        auto lg = lock_guard<mutex>{src_que_mutex};
        src_que.push(src);
      }else{
        delete src;
        auto lg = lock_guard<mutex>{src_que_mutex};
        src_que.push(nullptr);
        break;
      }
    }
  }};

  auto serial_th = thread{[&](){
    while(true){
      cv::Mat* src = nullptr;
      { 
        auto lg = lock_guard<mutex>{src_que_mutex};
        if(src_que.empty()){ continue; }
        src = src_que.front();
        src_que.pop();
      }
      if(src != nullptr){
        auto dst = new cv::Mat{};
        heavy(*src, *dst);
        delete src;
        {
          auto lg = lock_guard<mutex>{dst_que_mutex};
          dst_que.push(dst);
        }
      }else{
        auto lg = lock_guard<mutex>{dst_que_mutex};
        dst_que.push(nullptr);
        break;
      }
    }
  }};
  auto sink_th = thread{[&](){
    while(true){
      cv::Mat* dst = nullptr;
      { 
        auto lg = lock_guard<mutex>{dst_que_mutex};
        if(dst_que.empty()){ continue; }
        dst = dst_que.front();
        dst_que.pop();
      }
      if(dst != nullptr){
         writer.write(*dst);
         delete dst;
      }else{
        break;
      }
    }
  }};
  source_th.join();
  serial_th.join();
  sink_th.join();
  return EXIT_SUCCESS;
}
```

### boost::lockfree::queue を使う
* 並列処理のキューといえばこれ
* mutex を気にしなくてもよいのでかなり気が楽

```cpp
int main_lockfree(cv::VideoCapture& reader, cv::VideoWriter& writer){
  using std::thread;
  using boost::lockfree::queue;
  queue<cv::Mat*> src_que(128);
  queue<cv::Mat*> dst_que(128);
  auto source_th = thread([&]{
    while(true){
      cv::Mat* src = new cv::Mat{};
      if(reader.read(*src)){
        while(!src_que.push(src)){ "retry"; };
      }else{
        delete src;
        while(!src_que.push(nullptr)){ "retry"; }
        break;
      }
    }
  });
  auto serial_th = thread{[&](){
    while(true){
      cv::Mat* src = nullptr;
      while(!src_que.pop(src)){ "retry"; }
      if(src != nullptr){
        cv::Mat* dst = new cv::Mat{};
        heavy(*src, *dst);
        delete src;
        while(!dst_que.push(dst)){ "retry"; }
      }else{
        while(!dst_que.push(nullptr)){ "retry"; }
        break;
      }
    }
  }};
  auto sink_th = thread{[&](){
    while(true){
      cv::Mat* dst = nullptr;
      while(!dst_que.pop(dst)){ "retry"; }
      if(dst != nullptr){
        writer.write(*dst);
        delete dst;
      }else{
        break;
      }
    }
  }};
  source_th.join();
  serial_th.join();
  sink_th.join();
  return EXIT_SUCCESS;
}
```

### Intel® Threading Building Blocks を使う
* 巨人の肩の上に立つ

```cpp
class Source: public tbb::filter {
private:
  cv::VideoCapture reader;
public:
  Source(cv::VideoCapture& reader): filter(tbb::filter::mode::serial_in_order), reader{reader} {};
  void* operator()(void*){
    auto src = new cv::Mat{};
    if(reader.read(*src)){
      return src;
    }
    delete src;
    return nullptr;
  };
};

class SerialFilter: public tbb::filter {
public:
  SerialFilter(): filter(tbb::filter::mode::serial_in_order) {};
  void* operator()(void* ptr){
    auto src = static_cast<cv::Mat*>(ptr);
    auto dst = new cv::Mat{};
    heavy(*src, *dst);
    delete src;
    return dst;
  };
};

class Sink: public tbb::filter {
private:
  cv::VideoWriter writer;
public:
  Sink(cv::VideoWriter& writer): filter{tbb::filter::mode::serial_in_order}, writer{writer} {};
  void* operator()(void* ptr){
    auto dst = static_cast<cv::Mat*>(ptr);
    writer.write(*dst);
    delete dst;
    return nullptr;
  };
};

int main_tbb(cv::VideoCapture& reader, cv::VideoWriter& writer){
  tbb::task_scheduler_init init;
  tbb::pipeline pipe;
  auto source = Source{reader};
  auto filter = SerialFilter{};
  auto sink = Sink{writer};
  pipe.add_filter(source);
  pipe.add_filter(filter);
  pipe.add_filter(sink);
  pipe.run(4);
  return EXIT_SUCCESS;
}
```

## 結果

```console
root@70ce88779368:/data# time ./a.out --input big_buck_bunny_720p_30mb.mp4 --output naive.mp4 --mode 0


real    0m58.225s
user    1m29.259s
sys     0m8.369s
root@70ce88779368:/data# time ./a.out --input big_buck_bunny_720p_30mb.mp4 --output mutex.mp4 --mode 1

real    0m47.476s
user    2m13.875s
sys     0m12.271s
root@70ce88779368:/data# time ./a.out --input big_buck_bunny_720p_30mb.mp4 --output lockfree.mp4 --mode 2

real    0m49.268s
user    2m15.517s
sys     0m17.469s
root@70ce88779368:/data# time ./a.out --input big_buck_bunny_720p_30mb.mp4 --output tbb.mp4 --mode 3

real    0m48.485s
user    1m33.380s
sys     0m31.989s
```

## 考察

* 並列化した結果はほぼ同じ。
* tbb 使った時の user 時間が非パイプライン並列版とあまり変わらないのが謎

## まとめ

* 画像処理が重い場合、パイプライン並列でスループットを向上させられる
* 自分でキューとパイプラインを作るよりも tbb 使ったほうが楽
* 処理時間の比較、多段パイプライン処理の比較、スレッド数の比較などは今後の課題とする

## reference
* オープンソース化された並列化テンプレートクラスライブラリ「Intel Threading Building Blocks」入門 -
 https://mag.osdn.jp/09/08/21/1128207/6
* Intel® Threading Building Blocks - https://www.threadingbuildingblocks.org/docs/help/index.htm
* Boost.Lockfree - http://www.boost.org/doc/libs/1_63_0/doc/html/lockfree.html

## 付録

### 開発環境の Dockerfile

```sh
docker build -t cpp-tbb-pipeline ./
wget http://www.sample-videos.com/video/mp4/720/big_buck_bunny_720p_30mb.mp4
docker run -ti -v `pwd`:/data --workdir /data cpp-tbb-pipeline /bin/bash
```

```Dockerfile
FROM ubuntu:16.04
ENV DEBIAN_FRONTEND "noninteractive"

RUN apt-get update -y
RUN apt-get -y \
  -o Dpkg::Options::="--force-confdef" \
  -o Dpkg::Options::="--force-confold" dist-upgrade

RUN apt-get install -y --no-install-recommends \
  dconf-tools \
  apt-transport-https software-properties-common ppa-purge apt-utils \
  ca-certificates git curl wget \
  tar zip unzip zlib1g-dev bzip2 libbz2-dev \
  openssl libssl-dev \
  zsh vim screen tree htop \
  sudo

# Install gcc and clang
RUN wget -O - https://apt.llvm.org/llvm-snapshot.gpg.key | apt-key add -
RUN add-apt-repository "deb http://apt.llvm.org/xenial/ llvm-toolchain-xenial-5.0 main"
RUN add-apt-repository ppa:ubuntu-toolchain-r/test
RUN apt-get update -y
RUN apt-get install -y --no-install-recommends \
  build-essential binutils cmake autoconf automake autogen pkg-config libtool \
  gcc-6 g++-6 gcc-7 g++-7 gdb \
  clang-5.0 lldb-5.0 lld-5.0
RUN update-alternatives --install /usr/bin/gcc gcc /usr/bin/gcc-6 20
RUN update-alternatives --install /usr/bin/g++ g++ /usr/bin/g++-6 20

RUN apt-get install -y libboost-all-dev
RUN apt-get install -y libtbb2 libtbb-dev
RUN apt-get install -y libopenblas-dev liblapacke-dev
RUN apt-get install -y libeigen3-dev
RUN apt-get install -y ffmpeg libavcodec-dev libavformat-dev libswscale-dev
RUN apt-get install -y \
  gstreamer1.0-tools \
  gstreamer1.0-libav \
  gstreamer1.0-libav-dbg \
  gstreamer1.0-plugins-base \
  gstreamer1.0-plugins-good \
  gstreamer1.0-plugins-ugly \
  gstreamer1.0-plugins-bad \
  libgstreamer1.0-dev \
  libgstreamer-plugins-base1.0-dev \
  libgstreamer-plugins-good1.0-dev \
  libgstreamer-plugins-bad1.0-dev
WORKDIR /opt
RUN git clone --recursive --depth 1 https://github.com/opencv/opencv.git && \
cd opencv && \
mkdir -p build && \
cd build && \
cmake \
  -DCMAKE_BUILD_TYPE=RELEASE \
  -DBUILD_DOCS=OFF \
  -DBUILD_EXAMPLES=OFF \
  -DBUILD_TESTS=OFF \
  -DBUILD_PERF_TESTS=OFF \
  -DBUILD_WITH_DEBUG_INFO=OFF \
  -DBUILD_SHARED_LIBS=ON \
  -DWITH_OPENCL=ON \
  -DWITH_OPENGL=ON \
  -DENABLE_FAST_MATH=ON \
  -DWITH_EIGEN=ON \
  -DWITH_TBB=ON \
  -DWITH_PTHREADS_PF=ON \
  -DWITH_IPP=ON \
  -DWITH_FFMPEG=ON \
  -DWITH_GSTREAMER=ON \
  ../ && \
make -j && \
make install && \
ldconfig

RUN apt-get install -y -f
RUN apt-get update -y
RUN apt-get upgrade -y
RUN apt-get dist-upgrade -y

RUN apt-get clean -y
RUN apt-get autoremove -y
RUN apt-get autoclean -y
RUN rm -rf /var/lib/apt/lists/* /var/cache/apt/archives/*
```

## ソースコード

```c++
#include <iostream>
#include <cstdio>
#include <chrono>
#include <queue>
#include <mutex>
#include <thread>
#include <boost/program_options.hpp>
#include <boost/lockfree/queue.hpp>
#include <tbb/task_scheduler_init.h>
#include <tbb/pipeline.h>
#include <opencv2/opencv.hpp>
#include <opencv2/highgui/highgui.hpp>
#include <opencv2/imgproc.hpp>

using std::string;

void heavy(cv::Mat& src, cv::Mat& dst){
  cv::Mat mid1, mid2;
  cv::cvtColor(src, mid1, cv::COLOR_BGR2GRAY);
  cv::threshold(mid1, mid2, 0, 255, cv::THRESH_BINARY);
  cv::cvtColor(mid2, dst, cv::COLOR_GRAY2BGR);
  std::this_thread::sleep_for(std::chrono::milliseconds(10));
}

int main_naive(cv::VideoCapture& reader, cv::VideoWriter& writer){
  cv::Mat src;
  cv::Mat dst;
  while(reader.read(src)){
    heavy(src, dst);
    writer.write(dst);
  }
  return EXIT_SUCCESS;
}

int main_mutex(cv::VideoCapture& reader, cv::VideoWriter& writer){
  using std::thread;
  using std::mutex;
  using std::lock_guard;
  using std::queue;
  mutex src_que_mutex;
  queue<cv::Mat*> src_que;
  mutex dst_que_mutex;
  queue<cv::Mat*> dst_que;
  auto source_th = thread{[&](){
    while(true){
      auto src = new cv::Mat{};
      if(reader.read(*src)){
        auto lg = lock_guard<mutex>{src_que_mutex};
        src_que.push(src);
      }else{
        delete src;
        auto lg = lock_guard<mutex>{src_que_mutex};
        src_que.push(nullptr);
        break;
      }
    }
  }};
  auto serial_th = thread{[&](){
    while(true){
      cv::Mat* src = nullptr;
      { 
        auto lg = lock_guard<mutex>{src_que_mutex};
        if(src_que.empty()){ continue; }
        src = src_que.front();
        src_que.pop();
      }
      if(src != nullptr){
        auto dst = new cv::Mat{};
        heavy(*src, *dst);
        delete src;
        { 
          auto lg = lock_guard<mutex>{dst_que_mutex};
          dst_que.push(dst);
        }
      }else{
        auto lg = lock_guard<mutex>{dst_que_mutex};
        dst_que.push(nullptr);
        break;
      }
    }
  }};
  auto sink_th = thread{[&](){
    while(true){
      cv::Mat* dst = nullptr;
      { 
        auto lg = lock_guard<mutex>{dst_que_mutex};
        if(dst_que.empty()){ continue; }
        dst = dst_que.front();
        dst_que.pop();
      }
      if(dst != nullptr){
         writer.write(*dst);
         delete dst;
      }else{
        break;
      }
    }
  }};
  source_th.join();
  serial_th.join();
  sink_th.join();
  return EXIT_SUCCESS;
}

int main_lockfree(cv::VideoCapture& reader, cv::VideoWriter& writer){
  using std::thread;
  using boost::lockfree::queue;
  queue<cv::Mat*> src_que(128);
  queue<cv::Mat*> dst_que(128);
  auto source_th = thread([&]{
    while(true){
      cv::Mat* src = new cv::Mat{};
      if(reader.read(*src)){
        while(!src_que.push(src)){ "retry"; };
      }else{
        delete src;
        while(!src_que.push(nullptr)){ "retry"; }
        break;
      }
    }
  });
  auto serial_th = thread{[&](){
    while(true){
      cv::Mat* src = nullptr;
      while(!src_que.pop(src)){ "retry"; }
      if(src != nullptr){
        cv::Mat* dst = new cv::Mat{};
        heavy(*src, *dst);
        delete src;
        while(!dst_que.push(dst)){ "retry"; }
      }else{
        while(!dst_que.push(nullptr)){ "retry"; }
        break;
      }
    }
  }};
  auto sink_th = thread{[&](){
    while(true){
      cv::Mat* dst = nullptr;
      while(!dst_que.pop(dst)){ "retry"; }
      if(dst != nullptr){
        writer.write(*dst);
        delete dst;
      }else{
        break;
      }
    }
  }};
  source_th.join();
  serial_th.join();
  sink_th.join();
  return EXIT_SUCCESS;
}

class Source: public tbb::filter {
private:
  cv::VideoCapture reader;
public:
  Source(cv::VideoCapture& reader): filter(tbb::filter::mode::serial_in_order), reader{reader} {};
  void* operator()(void*){
    auto src = new cv::Mat{};
    if(reader.read(*src)){
      return src;
    }
    delete src;
    return nullptr;
  };
};

class SerialFilter: public tbb::filter {
public:
  SerialFilter(): filter(tbb::filter::mode::serial_in_order) {};
  void* operator()(void* ptr){
    auto src = static_cast<cv::Mat*>(ptr);
    auto dst = new cv::Mat{};
    heavy(*src, *dst);
    delete src;
    return dst;
  };
};

class Sink: public tbb::filter {
private:
  cv::VideoWriter writer;
public:
  Sink(cv::VideoWriter& writer): filter{tbb::filter::mode::serial_in_order}, writer{writer} {};
  void* operator()(void* ptr){
    auto dst = static_cast<cv::Mat*>(ptr);
    writer.write(*dst);
    delete dst;
    return nullptr;
  };
};

int main_tbb(cv::VideoCapture& reader, cv::VideoWriter& writer){
  tbb::task_scheduler_init init;
  tbb::pipeline pipe;
  auto source = Source{reader};
  auto filter = SerialFilter{};
  auto sink = Sink{writer};
  pipe.add_filter(source);
  pipe.add_filter(filter);
  pipe.add_filter(sink);
  pipe.run(4);
  return EXIT_SUCCESS;
}

int main(int argc, char* argv[]){
  string INPUT_VIDEO_PATH;
  string OUTPUT_VIDEO_PATH;
  int PIPELINE_MODE;
  namespace po = boost::program_options;
  po::options_description opt("option");
  opt.add_options()
    ("input,i", po::value<string>(&INPUT_VIDEO_PATH)->required(), "input video file path")
    ("output,o", po::value<string>(&OUTPUT_VIDEO_PATH)->required(), "output mp4 video file path if you need")
    ("mode", po::value<int>(&PIPELINE_MODE)->default_value(0), "0: naive, 1: mutex, 2: lockfree, 3: tbb mode")
    ("help,h", "show this menu");
  auto vm = po::variables_map{};
  try{
    po::store(po::parse_command_line(argc, argv, opt), vm);
    po::notify(vm);
  }catch(std::exception& e){
    std::cerr << "Error: " << e.what() << "\n";
    std::cout << opt << std::endl; // put help
    return EXIT_FAILURE;
  }
  if( vm.count("help")  ){
    std::cout << opt << std::endl; // put help
    return EXIT_FAILURE;
  }

  auto reader = cv::VideoCapture{ INPUT_VIDEO_PATH };
  if(!reader.isOpened()){
    printf("C++: reader cannot opened\n");
    return EXIT_FAILURE;
  }
  int width = reader.get(CV_CAP_PROP_FRAME_WIDTH);
  int height = reader.get(CV_CAP_PROP_FRAME_HEIGHT);
  double fps = reader.get(CV_CAP_PROP_FPS);

  auto writer = cv::VideoWriter{ "appsrc ! videoconvert ! x264enc ! mp4mux ! filesink location=" + OUTPUT_VIDEO_PATH + "  ", 0, fps, cv::Size{ width, height }, true};

  switch(PIPELINE_MODE){
    case 0: return main_naive(reader, writer);
    case 1: return main_mutex(reader, writer);
    case 2: return main_lockfree(reader, writer);
    case 3: return main_tbb(reader, writer);
  }
}

```

```sh
g++-7 main.cpp -std=c++1z -O3 -Wall -v `pkg-config --libs opencv` -lboost_program_options -ltbb -lpthread

time ./a.out --input big_buck_bunny_720p_30mb.mp4 --output naive.mp4 --mode 0
time ./a.out --input big_buck_bunny_720p_30mb.mp4 --output mutex.mp4 --mode 1
time ./a.out --input big_buck_bunny_720p_30mb.mp4 --output lockfree.mp4 --mode 2
time ./a.out --input big_buck_bunny_720p_30mb.mp4 --output tbb.mp4 --mode 3
```

