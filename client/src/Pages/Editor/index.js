import 'es6-promise/auto'
import './Main.css'
import './Stylesheets.elm'
import initCodeMirror from '../../Views/Editors/CodeMirror'
import fixHtml from './fixHtml'
import captureOpbeat from '../../Shared/Opbeat'

const vimMode = window.location.search.indexOf('vim=true') !== -1
initCodeMirror(vimMode)
  .then(() => {
    const Elm = require('./Main.elm')
    let hasUnsavedWork = false;
    let previousLocation = window.location.pathname
    window.addEventListener('popstate', e => {
      if (hasUnsavedWork) {
        const result = window.confirm('You have unsaved work. Are you sure you want to go?')
        if (!result) {
          window.history.pushState({}, '', previousLocation)
          e.preventDefault()
        }
      }
    })

    const app = Elm.Pages.Editor.Main.fullscreen({
      windowSize: {
        width: window.innerWidth,
        height: window.innerHeight
      },
      vimMode: vimMode,
      online: process.env.NODE_ENV === 'production' ? window.navigator.onLine : true
    })

    app.ports.opbeatCaptureOut.subscribe(captureOpbeat)

    app.ports.pathChangedOut.subscribe(() => {
      previousLocation = window.location.pathname
    })

    app.ports.hasUnsavedWork.subscribe(nextValue => {
      hasUnsavedWork = nextValue
    })

    app.ports.reloadIframeOut.subscribe(() => {
      var iframe = document.getElementById('results_iframe')
      if (!iframe) return
      iframe.src = iframe.src
    })

    app.ports.openDebuggerOut.subscribe(() => {
      var iframe = document.getElementById('results_iframe')
      if (!iframe) return
      iframe.contentWindow.postMessage({ type: 'debug' }, API_ORIGIN)
    })

    app.ports.openNewWindow.subscribe(url => {
      var win = window.open(url, '_blank')
      win.focus()
    })

    window.addEventListener('online', function () {
      app.ports.online.send(true)
    })

    window.addEventListener('offline', function () {
      app.ports.online.send(false)
    })

    window.addEventListener('beforeunload', function (e) {
      if (hasUnsavedWork) {
        e.returnValue = 'You have unsaved work. Are you sure you want to go?'
      }
    })

    window.addEventListener('message', function (event) {
      if (event.data.type === 'error') {
        app.ports.jsError.send(event.data.message)
        return
      }

      app.ports.windowMessageIn.send(event.data)
    })


    import(/* webpackChunkName: "make-0.18.0" */'Make/0.18.0')
      .then(Compiler => {
        let workQueue = []
        let runForSave, htmlCode

        const readFile = (file) =>
          new Promise((resolve, reject) => {
            const fr = new FileReader()
            fr.addEventListener('load', () => {
              resolve(fr.result)
            })
            fr.addEventListener('error', () => {
              reject(fr.error)
            })
            fr.readAsText(file)
          })

        const getSourceScript = (scriptFile) => {
          if (runForSave) {
            return readFile(scriptFile)
              .then(data => '<script>' + data + '</script>')
          } else {
            return Promise.resolve(`<script src=${URL.createObjectURL(scriptFile)}></script>`)
          }
        }

        const sendToPort = data => {
          console.log(data)
          runForSave ?
            app.ports.compileForSaveIn.send(data) :
            app.ports.compilerMessagesIn.send(data)
        }

        const callback = (data) => {
          workQueue.push(data)
          setTimeout(function work() {
            if (!workQueue.length) return
            var nextMessage = workQueue.shift()

            if (nextMessage.type === 'Success') {
              getSourceScript(nextMessage.url)
                .then(sourceScript => fixHtml({
                  htmlCode,
                  embedApi: !runForSave,
                  sourceScript
                }))
                .then(htmlUrl => sendToPort({ type: 'Success', url: htmlUrl }))
                .catch(error => sendToPort({ type: 'Failed', message: error.message }))
            } else {
              sendToPort(nextMessage)
            }
            setTimeout(work)
          })
        }

        const compile = Compiler.init(callback)

        app.ports.compileOnClientOut.subscribe(function ([html, elm, packages, forSave]) {
          runForSave = forSave
          htmlCode = html
          compile({ elm, packages })
        })
      })


  })
