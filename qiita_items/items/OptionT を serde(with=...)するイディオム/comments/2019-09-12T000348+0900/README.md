型が変わっても良いのであれば，以下でも動くかもしれません．

```rust
#[derive(Serialize, Deserialize)]
pub struct Job {
    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(default)]
    pub ended_at: Option<DateTimeUtcWrapper>,
}

#[derive(Serialize, Deserialize)]
pub struct DateTimeUtcWrapper(
    #[serde(with = "chrono::serde::ts_milliseconds")]
    DateTime<Utc>
);

impl From<DateTimeUtcWrapper> for DateTime<Utc> {
    fn from(wrapper: DateTimeUtcWrapper) -> Self { wrapper.0 }
}
```

一時的な型に変換する場所が違うだけで，基本的にやっていることは同じです．Serializer/Deserializerの実装コードを書かなくてよくなりますが，Wrapperから実際の型への変換が必要になります．

上記は動作未確認ですが，少なくとも以下のような`Deserialize`のみの場合は動作しました．

```rust
#[derive(Deserialize)]
struct StreamQuery {
    #[serde(default)]
    decode: u8,  // default: 0
    #[serde(default)]
    duration: Option<DurationWrapper>,
}

// serde_duration_in_millisは，chrono::Durationとミリ秒の数値（i64）の間の変換を実装したモジュール
#[derive(Deserialize)]
struct DurationWrapper(#[serde(with = "serde_duration_in_millis")] Duration);

impl From<DurationWrapper> for Duration {
    fn from(wrapper: DurationWrapper) -> Self { wrapper.0 }
}
```

動作確認は，`actix_web::web::Query` Extractorで行いました．
