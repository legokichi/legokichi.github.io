2019-08-01 追記 [rust-embedded/cross](https://github.com/rust-embedded/cross/) を使えばいいです

```bash
cargo install --force --git https://github.com/rust-embedded/cross cross
git clone https://github.com/rust-embedded/cross
pushd cross
docker build -t arm-unknown-linux-musleabihf:latest -f docker/arm-unknown-linux-musleabihf/Dockerfile docker
popd
cat << 'EOF' > Cross.toml
[target.arm-unknown-linux-musleabihf]
image = "arm-unknown-linux-musleabihf:latest"
EOF
cross build --target arm-unknown-linux-musleabihf
```
