# fetch API でも POST したい！

ちなみに fetch API は WHATWG HTML Living Standard で定義されていて、W3C HTML 5.1 でもなければ ECMAScript2017 でもないです。

2017年現在 Safari や IE では未サポートなので https://github.com/github/fetch などの polyfill を使うと良いです。

## post (application/x-www-form-urlencoded)

```js
const obj = {hello: "world"};
const method = "POST";
const body = Object.keys(obj).map((key)=>key+"="+encodeURIComponent(obj[key])).join("&");
const headers = {
  'Accept': 'application/json',
  'Content-Type': 'application/x-www-form-urlencoded; charset=utf-8'
};
fetch("./new", {method, headers, body}).then((res)=> res.json()).then(console.log).catch(console.error);
```

## post (multipart/form-data)

```js
const obj = {hello: "world"};
const method = "POST";
const body = Object.keys(obj).reduce((o,key)=>(o.set(key, obj[key]), o), new FormData());
const headers = {
  'Accept': 'application/json'
};
fetch("./new", {method, headers, body}).then((res)=> res.json()).then(console.log).catch(console.error);
```

## post (aplication/json)

```js
const obj = {hello: "world"};
const method = "POST";
const body = JSON.stringify(obj);
const headers = {
  'Accept': 'application/json',
  'Content-Type': 'application/json'
};
fetch("./new", {method, headers, body}).then((res)=> res.json()).then(console.log).catch(console.error);
```
