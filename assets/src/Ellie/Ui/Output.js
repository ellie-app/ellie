import CustomElements from '../../Platform/CustomElements'

export default {
  start(app) {
    app.ports.ellieUiOutputOut.subscribe((data) => {
      switch (data.tag) {
        case 'Reload':
          requestAnimationFrame(() => {
            document.querySelectorAll('ellie-ui-output[data-connected]').forEach((el) => {
              el.reload()
            })
          })
          return
      }
    })

    const parser = new DOMParser()
    const iframe = document.createElement('iframe')

    const embeddedApi =
    `(function () {
      var parent = window.parent
      delete window.parent

      // LOGS
    
      var oldLog = console.log
      console.log = function () {
        var firstArg = arguments[0]
        if (arguments.length === 1 && typeof firstArg === 'string' && firstArg.indexOf(': ') !== -1) {
          var split = firstArg.split(': ')
          var label = split[0]
          var body = split.slice(1).join(': ')
          parent.postMessage({ tag: 'LogReceived', contents: { label: label, body: body } }, '${window.location.origin}')
        }
        oldLog.apply(this, arguments)
      }
    
      // DEBUGGER
      var debuggerWindowProxy = null
      var iframe = document.createElement('iframe')
      window.open = function () {
        iframe.style.display = 'block'
        iframe.style.zIndex = 2147483648
        iframe.style.width = '100%'
        iframe.style.height = '100%'
        iframe.style.position = 'fixed'
        iframe.style.top = 0
        iframe.style.left = 0
        iframe.style.border = 0
        iframe.style.backgroundColor = '#fff'
        iframe.src = 'javascript:void(0);'
        document.body.appendChild(iframe)
        var onClose = function () {}
        debuggerWindowProxy = Object.defineProperties({}, {
          document: {
            get: function () { return iframe.contentDocument }
          },
          close: {
            value: function () {
              iframe.style.display = 'none'
              onClose()
            }
          },
          addEventListener: {
            value: function (event, fn) {
              if (event === 'unload') onClose = fn
            }
          }
        })
        return debuggerWindowProxy
      }
      var buttonSelector = 'div[style*="z-index: 2147483647"][style*="background-color: rgb(61, 61, 61)"] > div[style*="cursor: pointer"], .elm-mini-controls-button'
      var controlsSelector = 'div[style*="z-index: 2147483647"][style*="background-color: rgb(61, 61, 61)"], .elm-mini-controls'
      var overlaySelector = 'div[style*="z-index: 2147483646"][style*="background-color: rgba(200, 200, 200, 0.7)"]'
      var styles = document.createElement('style')
      // styles.textContent = controlsSelector + ' { display: none !important; }'
      document.head.appendChild(styles)
      window.addEventListener('message', function (event) {
        switch(event.data.tag) {
          case 'SwitchToDebugger':
            var button = document.querySelector(buttonSelector)
            if (button) button.click()
            break
    
          case 'SwitchToProgram':
            if (debuggerWindowProxy) debuggerWindowProxy.close()
            setTimeout(function () {
              var overlay = document.querySelector(overlaySelector)
              if (overlay) overlay.id = 'elm-debugger-overlay'
            }, 32)
            break
        }
      })

      window.addEventListener('error', function (e) {
        parent.postMessage({
          tag: 'RuntimeException',
          contents: { message: e.message }
        }, '${window.location.origin}')
      })

      // READY
      document.addEventListener('DOMContentLoaded', function () {
        setTimeout(function () {
          parent.postMessage({
            tag: 'Ready',
            contents: { canDebug: document.querySelector(buttonSelector) !== null }
          }, '${window.location.origin}')
        }, 100)
      })
    }())`

    const wrapOnReady = (random, script) => {
      return '__ellie_onReady_' + random + '(function () {' + script + '})'
    }

    const embedApi = (doc) => {
      const script = doc.createElement('script')
      script.textContent = embeddedApi
      const style = doc.createElement('style')
      style.textContent = `
        div[style*="z-index: 2147483646"][style*="background-color: rgba(200, 200, 200, 0.7)"] {
          position: fixed;
          top: 0;
          left: 0;
        }
        .elm-overlay {
          z-index: 2147483646 !important;
        }
        .elm-mini-controls {
          display: none !important;
        }`
      doc.head.appendChild(script)
      doc.head.appendChild(style)
    }

    const onReady = (doc, marker) => {
      const script = doc.createElement('script')
      script.textContent =
        `function __ellie_onReady_${marker}(cb) {
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

    const fixHtml = (htmlCode, scriptSrc) => {
      const doc = parser.parseFromString(htmlCode, 'text/html')

      const random = Math.floor(9007199254740991 * Math.random())

      doc.querySelectorAll('script').forEach(el => {
        if (el.getAttribute('src') !== null && !el.textContent.trim()) return
        el.textContent = wrapOnReady(random, el.textContent)
      })

      embedApi(doc)
      onReady(doc, random)

      const elmScript = doc.createElement('script')
      elmScript.src = scriptSrc
      doc.head.appendChild(elmScript)

      const charset = doc.createElement('meta')
      charset.setAttribute('charset', 'utf-8')
      doc.head.insertBefore(charset, doc.head.children[0])

      const finalHtml = '<!DOCTYPE html>' + doc.documentElement.outerHTML
      var blob = new Blob([finalHtml], { type: 'text/html' })
      return URL.createObjectURL(blob)
    }

    // const convertStringToArrayBufferView = (string) => {
    //   return new Promise((resolve) => {
    //     requestIdleCallback(() => {
    //       const length = string.length
    //       const bytes = new Uint8Array(length)
    //       for (var i = 0; i < length; i++) {
    //         bytes[i] = string.charCodeAt(i)
    //       }
    //       resolve(bytes)
    //     })
    //   })
    // }

    // const convertArrayBufferToHexaDecimal = (buffer) => {
    //   var data_view = new DataView(buffer)
    //   var iii, len, hex = '', c
    //   for(iii = 0, len = data_view.byteLength; iii < len; iii += 1) {
    //     c = data_view.getUint8(iii).toString(16)
    //     if (c.length < 2) c = '0' + c
    //     hex += c
    //   }
    //   return hex
    // }

    CustomElements.define('ellie-ui-output', (HTMLElement) => class extends HTMLElement {
      constructor() {
        super()
        this._html = null
        this._elmSource = null
        this._debug = false
        this._url = null
        this._attached = false
        this._onMessage = this._onMessage.bind(this)
        this._idleCallback = null
      }

      get html() {
        return this._html
      }

      set html(value) {
        if (this._html === value) return
        this._html = value
        if (!this._attached) return
        this._update()
      }

      get elmSource() {
        return this._elmSource
      }

      set elmSource(value) {
        if (this._elmSource === value) return
        this._elmSource = value
        if (!this._attached) return
        this._update()
      }

      get debug() {
        return this._debug
      }

      set debug(value) {
        this._debug = value
        if (!this._attached) return
        if (value) {
          iframe.contentWindow.postMessage({ tag: 'SwitchToDebugger' }, window.location.origin)
        } else {
          iframe.contentWindow.postMessage({ tag: 'SwitchToProgram' }, window.location.origin)
        }
      }

      connectedCallback() {
        this.appendChild(iframe)
        this._attached = true
        window.addEventListener('message', this._onMessage)
        iframe.addEventListener('load', this._onLoad)
        this._update()
        this.setAttribute('data-connected', '')
      }

      disconnectedCallback() {
        window.removeEventListener('message', this._onMessage)
        this.removeChild(iframe)
        cancelIdleCallback(this._idleCallback)
        this.removeAttribute('data-connected')
      }

      reload() {
        iframe.src = iframe.src
      }

      _onMessage(e) {
        switch (e.data.tag) {
          case 'LogReceived':
            this.dispatchEvent(new CustomEvent('log', { detail: e.data.contents }))
            return
          case 'Ready':
            this._canDebug = e.data.contents.canDebug
            this.dispatchEvent(new CustomEvent('canDebug', { detail: e.data.contents.canDebug }))
            this._onReady()
            return
          case 'RuntimeException':
            this.dispatchEvent(new CustomEvent('runtimeException', { detail: e.data.contents.message }))
        }
      }

      _onReady() {
        if (this._debug && this._canDebug) {
          iframe.contentWindow.postMessage({ tag: 'SwitchToDebugger' }, window.location.origin)
        } else {
          iframe.contentWindow.postMessage({ tag: 'SwitchToProgram' }, window.location.origin)
        }
      }

      _update() {
        cancelIdleCallback(this._idleCallback)
        this._idleCallback = requestIdleCallback(() => {
          URL.revokeObjectURL(this._url)
          this._url = fixHtml(this._html, this._elmSource)
          iframe.src = this._url
        })
      }
    })
  }
}
