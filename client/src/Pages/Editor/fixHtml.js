const embeddedApi = (doc) => {
  const script = doc.createElement('script')
  script.textContent =
`(function () {
  var open = window.open
  var childWindows = []
  window.open = function () { var win = open.apply(window, arguments); childWindows.push(win); return win; }
  window.addEventListener("beforeunload", function() { childWindows.forEach(function(win) { win.close() }) })
  var origin = '${window.location.origin}'
  var parent = window.parent
  document.addEventListener("mouseup", function () {
    parent.postMessage({ type: "mouseup" }, origin)
  })
  document.addEventListener("mousemove", function (e) {
    parent.postMessage({ type: "mousemove", x: e.screenX, y: e.screenY }, origin)
  })
  document.addEventListener("click", function (e) {
    parent.postMessage({ type: "click" }, origin)
  })
  window.addEventListener("error", function (e) {
    parent.postMessage({ type: "error", message: e.error.message }, origin)
  })
  window.addEventListener("message", function (e) {
    if (e.data.type === "debug") {
      var button = document.querySelector(".elm-mini-controls-button")
      if (button) button.click()
    }
  })

  var oldLog = console.log
  console.log = function () {
    var firstArg = arguments[0]
    if (arguments.length === 1 && typeof firstArg === 'string' && firstArg.indexOf(':') !== -1) {
      var split = firstArg.split(': ')
      var label = split[0]
      var value = split.slice(1).join(': ')
      parent.postMessage({ type: 'log', label: label, value: value }, origin)
    }
    oldLog.apply(this, arguments)
  }
  delete window.parent
}())`

  const style = doc.createElement('style')
  style.textContent =
`.elm-overlay { z-index: 999999999999 !important; }
.elm-mini-controls { display: none !important; }`

  doc.head.appendChild(script)
  doc.head.appendChild(style)
}

const onReady = (doc, random) => {
  const script = doc.createElement('script')
  script.textContent =
`function __ellie_onReady_${random}(cb) {
  function completed() {
    document.removeEventListener("DOMContentLoaded", completed)
    window.removeEventListener("load", completed)
    cb()
  }
  if (document.readyState === "complete" || ( document.readyState !== "loading" && !document.documentElement.doScroll)) {
    setTimeout(completed)
  } else {
    document.addEventListener("DOMContentLoaded", completed)
    window.addEventListener("load", completed)
  }
}`
  doc.head.appendChild(script)
}

const wrapOnReady = (random, script) => {
  return '__ellie_onReady_' + random + '(function () {' + script + '})'
}

const parser = new DOMParser()

export default ({ htmlCode, embedApi, sourceScript }) => {
  return new Promise(function (resolve, reject) {
    try {
      const doc = parser.parseFromString(htmlCode, 'text/html')
      const random = Math.floor(9007199254740991 * Math.random())

      doc.querySelectorAll('script').forEach(el => {
        if (el.getAttribute('src') !== null && !el.textContent.trim()) return
        el.textContent = wrapOnReady(random, el.textContent)
      })

      if (embedApi) embeddedApi(doc)
      onReady(doc, random)

      const div = doc.createElement('div')
      div.innerHTML = sourceScript
      doc.body.prepend(div.children[0])
      var blob = new Blob([doc.documentElement.outerHTML], { type: 'text/html' })
      resolve(URL.createObjectURL(blob))
    } catch (error) {
      reject(error)
    }
  })
}
