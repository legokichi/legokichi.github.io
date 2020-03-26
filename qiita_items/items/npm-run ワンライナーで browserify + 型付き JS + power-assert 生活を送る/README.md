# npm-run ワンライナーで型付きJS生活を送る

grunt とか gulp とか webpack とか設定が面倒くさいので npm-run だけですべてを済ませたい。

```ｓｈ
npm run build # ビルド
npm run watch # 差分ビルド
npm run lint  # 構文チェック
npm run check # 型チェック
npm run test  # テスト
```

だけで生きていこうという話です。



## TypeScript の場合

tsc の差分ビルドと watchify の差分ビルドを両方有効にするために lib フォルダを途中に挟んでいます。

ファイルツリー

```sh
.
├── dist
│   └── index.js # es5 + browserify
├── lib
│   └── index.js # es5 + commonjs
├── src
│   └── index.ts # typescript
├── tsconfig.json
├── tslint.json
├── typings
│   └── index.d.ts
└── typings.json
```

package.json

```json:package.json
{
  "scripts": {
    "build": "tsc    -p . && browserify lib/index.js --standalone Main -o dist/index.js -t espowerify",
    "watch": "tsc -w -p .  &   watchify lib/index.js --standalone Main -o dist/index.js -t espowerify -v",
    "check": "tsc -w --noEmit -p ./",
    "lint": "tslint -c ./tslint.json --project ./tsconfig.json --type-check"
  },
  "devDependencies": {
    "browserify": "^13.1.0",
    "espowerify": "^1.0.0",
    "power-assert": "^1.4.1",
    "typescript": "^2.0.3",
    "watchify": "^3.7.0"
  }
}
```

tsconfig.json で tsc を es6 にするなら babelify を入れる。

## babel + flowtype の場合

ファイルツリー

```sh
.
├── dist
│   └── index.js # es5 + browserify
├── src
│   ├── decls
│   │   ├── externs.js
│   │   └── globals.js
│   └── index.js # babel + flowtype
├── .eslintrc.json
├── .flowconfig
└── package.json
```

package.json

```json:package.json
{
 "scripts": {
    "build": "browserify src/index.js --standalone Main -o dist/index.js -t [ babelify --presets [ es2015 ] --plugins [ babel-plugin-transform-flow-strip-types ] ] -t espowerify",
    "watch": "  watchify src/index.js --standalone Main -o dist/index.js -t [ babelify --presets [ es2015 ] --plugins [ babel-plugin-transform-flow-strip-types ] ] -t espowerify -v",
    "check": "flow check",
    "lint": "eslint ./src"
  },
  "devDependencies": {
    "babel-plugin-transform-flow-strip-types": "^6.14.0",
    "babel-preset-es2015": "^6.16.0",
    "babelify": "^7.3.0",
    "browserify": "^13.1.0",
    "eslint": "^3.7.1",
    "espowerify": "^1.0.0",
    "flow-bin": "^0.33.0",
    "power-assert": "^1.4.1",
    "watchify": "^3.7.0"
  }
}
```

## 参考
* http://qiita.com/mizchi/items/95ee0101ac22e4b7b662
