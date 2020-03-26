![iframe](https://i.gyazo.com/29d9a92413fc5fd8f4e94306488288de.gif)

こんなことがしたい。

# react の場合
```html
<body>
  <iframe id="box-preview-iframe"></iframe>
</body>
```

```coffeescript
$ ->
  iframeBody = document.getElementById("box-preview-iframe").contentDocument.body
  comp = React.render(React.createElement(Preview, {}), iframeBody)
  setInterval((()->
    date = new Date
    console.log body = """<div>
    <span>#{date.getHours()}</span>:
    <span>#{date.getMinutes()}</span>:
    <span>#{date.getSeconds()}</span>
    </div>"""
    React.render(React.createElement(Preview, {body}), iframeBody)
  ),1000)

Preview = React.createClass
  render: ->
    JSXTransformer.exec(@props.body || "<div></div>")
```

## [追記] Mithril の場合
* https://github.com/lhorie/mithril.js/issues/67
* https://lhorie.github.io/mithril/tools.html
* https://lhorie.github.io/mithril/tools/template-converter.html

を読む限り

* http://lhorie.github.io/mithril/tools/template-converter.js

これを使えばよさそう。component含んでるけど。

```html
<script src="template-converter.js"></script>
<body>
</body>
```

```coffeescript
$ ->
  m.mount(document.body, RootComponent)

RootComponent =
  controller: (data)-> {}
  view: (ctrl)->
    m("div", {id: "box"}, [
      m("section", {id: "box-box-preview"}, [
        m.component(PreviewComponent, ctrl.PreviewController)
      ])
    ])


PreviewComponent =
  controller: (attrs)->
    setInterval((()->
      console.log date = new Date
      model.body("""<div>
      <span>#{date.getHours()}</span>:
      <span>#{date.getMinutes()}</span>:
      <span>#{date.getSeconds()}</span>
      </div>""")
      m.redraw(true)
    ),1000)
    model = {
      head: m.prop("")
      body: m.prop("")
    }
  view: (ctrl, attrs)->
    m("iframe", {
      id: "box-preview-iframe",
      config: PreviewComponent.config(ctrl, attrs)
    }, [])
  config: (ctrl, attrs)-> (elm, isInitialized, ctx, vdom)=>
    if !isInitialized
      console.log ctrl
      m.mount(elm.contentDocument.head, {
        view: (_ctrl, _attrs)->
          code = templateConverter.Template(ctrl.head())
          new Function("ctrl", "attrs", "return #{code}")(_ctrl, _attrs)
      })
      m.mount(elm.contentDocument.body, {
        view: (_ctrl, _attrs)->
          code = templateConverter.Template(ctrl.body())
          new Function("ctrl", "attrs", "return #{code}")(_ctrl, _attrs)
      })

```

![mithril](https://i.gyazo.com/f36bd8f6dc645997b136fd6ed99e5b44.gif)
