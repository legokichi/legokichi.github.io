# `Option<T>` を `serde(with="...")` するイディオム

```ts
type UnixTimeMillis = number;

interface Job {
    started_at: UnixTimeMillis;
    ended_at?: UnixTimeMillis;
}
```

のような JSON を [`chrono::serde::ts_milliseconds`](https://docs.rs/chrono/0.4.6/chrono/serde/ts_milliseconds/index.html) でシリアライズ｜デシリアライズしたい。

直感的に書くと以下のようになるが、 `ended_at` の `???` の部分はどうしたらよいのか、という問題が起きる。

```rust
#[derive(Serialize, Deserialize)]
pub struct Job {
    #[serde(with = "::chrono::serde::ts_milliseconds")]
    pub started_at: DateTime<Utc>,
    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(with = "???")]
    pub ended_at: Option<DateTime<Utc>>,
}
```

現状の serde では [Option などにラップされた型を with を使ったカスタムシリアライザで扱うことができない](https://github.com/serde-rs/serde/issues/723) ためである。

そこで、 [以下のような `Helper` 構造体を介してパースしてやると良い](https://github.com/serde-rs/serde/issues/1301#issuecomment-394108486) 。

```rust
#[derive(Serialize, Deserialize)]
pub struct Job {
    #[serde(with = "::chrono::serde::ts_milliseconds")]
    pub started_at: DateTime<Utc>,
    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(with = "option_ts_milliseconds")]
    #[serde(default="default_opt_date")]
    pub ended_at: Option<DateTime<Utc>>,
}

fn default_opt_date() -> Option<DateTime<Utc>> {
    None
}

mod option_ts_milliseconds {
    use serde_derive::*;
    use chrono::{Utc, DateTime};
    use serde::ser::{Serialize, Serializer};
    use serde::de::{Deserialize, Deserializer};
    
    pub fn serialize<S>(value: &Option<DateTime<Utc>>, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        #[derive(Serialize)]
        struct Helper<'a>(#[serde(with = "::chrono::serde::ts_milliseconds")] &'a DateTime<Utc>);
        value.as_ref().map(Helper).serialize(serializer)
    }
    pub fn deserialize<'de, D>(deserializer: D) -> Result<Option<DateTime<Utc>>, D::Error>
    where
        D: Deserializer<'de>,
    {
        #[derive(Deserialize)]
        struct Helper(#[serde(with = "::chrono::serde::ts_milliseconds")] DateTime<Utc>);
        let helper = Option::deserialize(deserializer)?;
        Ok(helper.map(|Helper(o)| o))
    }
}
```

__※ `skip_serializing_if` を使う場合は Default で None を返すようにしてやる必要がある__

## 参考
* Using de/serialize_with inside of an Option, Map, Vec - https://github.com/serde-rs/serde/issues/723
* (De)serialize remote struct with Option<T> fields - https://github.com/serde-rs/serde/issues/1301#issuecomment-394108486
