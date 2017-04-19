var Cheerio = require('cheerio')
var Memoize = require('./Memoize')

var embeddedApi =
  '<script>\n' +
  '(function () {\n' +
    'var open = window.open\n' +
    'var childWindows = []\n' +
    'window.open = function () { var win = open.apply(window, arguments); childWindows.push(win); return win; }\n' +
    'window.addEventListener("beforeunload", function() { childWindows.forEach(function(win) { win.close() }) })\n' +
    'var origin = "' + self.location.origin + '"\n' +
    'var parent = window.parent\n' +
    'document.addEventListener("mouseup", function () {\n' +
     'parent.postMessage({ type: "mouseup" }, origin)\n' +
    '})\n' +
    'document.addEventListener("mousemove", function (e) {\n' +
     'parent.postMessage({ type: "mousemove", x: e.screenX, y: e.screenY }, origin)\n' +
    '})\n' +
    'document.addEventListener("click", function (e) {\n' +
     'parent.postMessage({ type: "click" }, origin)\n' +
    '})\n' +
    'window.addEventListener("error", function (e) {\n' +
      'parent.postMessage({ type: "error", message: e.error.message }, origin)\n' +
    '})\n' +
    'window.addEventListener("message", function (e) {\n' +
      'if (e.data.type === "debug") {\n' +
        'var button = document.querySelector(".elm-mini-controls-button")\n' +
        'if (button) button.click()\n' +
      '}\n' +
    '})\n' +
    'delete window.parent\n' +
  '}())\n' +
  '</script>\n' +
  '<style>\n' +
    '.elm-overlay { z-index: 999999999999 !important; }\n' +
    '.elm-mini-controls { display: none !important; }\n' +
  '</style>'

function onReady(random) {
  return '<script>\n' +
    'function __ellie_onReady_' + random + '(cb) {\n' +
      'function completed() {\n' +
        'document.removeEventListener("DOMContentLoaded", completed)\n' +
        'window.removeEventListener("load", completed)\n' +
        'cb()\n' +
      '}\n' +
      'if (document.readyState === "complete" || ( document.readyState !== "loading" && !document.documentElement.doScroll)) {\n' +
        'setTimeout(completed)\n' +
      '} else {\n' +
        'document.addEventListener("DOMContentLoaded", completed)\n' +
        'window.addEventListener("load", completed)\n' +
      '}\n' +
    '}\n' +
    '</script>'
}

function wrapOnReady(random, script) {
  return '__ellie_onReady_' + random + '(function () {' + script + '})'
}

function fix(inputs) {
  return new Promise(function (resolve, reject) {
    try {
      var $ = Cheerio.load(inputs.htmlCode)
      var needsHtml = $('html').length === 0
      var needsHead = $('head').length === 0
      var needsBody = $('body').length === 0

      var random = Math.floor(9007199254740991 * Math.random())

      if (needsBody) {
        var _$ = Cheerio.load('<body></body>')
        _$('body').append($.html())
        $ = _$
      }

      if (needsHtml) {
        var _$ = Cheerio.load('<html></html>')
        _$('html').append($.html())
        $ = _$
      }

      if (needsHead) $('html').prepend('<head></head>')

      $('script').each((i, el) => {
        var children = el.children
        if (children.length === 0) return
        var firstChild = children[0]
        firstChild.data = wrapOnReady(random, firstChild.data)
      })

      if (inputs.embedApi) $('head').append(embeddedApi)
      $('head').append(onReady(random))

      $('body').prepend(inputs.sourceScript)

      var blob = new Blob([$.html()], { type: 'text/html' })
      resolve(URL.createObjectURL(blob))
    } catch (error) {
      reject(error)
    }
  })
}

module.exports = {
  fix: Memoize.memoize(fix)
}
