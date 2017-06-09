import 'es6-promise/auto'
import './Main.css'
import '../../../elm/Apps/Editor/Stylesheets.elm'
import initCodeMirror from '../../Shared/CodeMirror'

const vimMode = window.location.search.indexOf('vim=true') !== -1
initCodeMirror(vimMode)
  .then(() => {
    const Elm = require('../../../elm/Apps/Editor/Main.elm')
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

    const app = Elm.Apps.Editor.Main.fullscreen({
      windowSize: {
        width: window.innerWidth,
        height: window.innerHeight
      },
      vimMode: vimMode,
      online: process.env.NODE_ENV === 'production' ? window.navigator.onLine : true
    })

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

    import('../../Shared/Compiler')
      .then(Compiler => {
        var workQueue = []
        let runForSave

        const callback = (data) => {
          console.log(data)
          workQueue.push(data)
          setTimeout(function work() {
            if (!workQueue.length) return
            var next = workQueue.shift()
            if (runForSave) {
              app.ports.compileForSaveIn.send(next)
            } else {
              app.ports.compilerMessagesIn.send(next)
            }
            setTimeout(work)
          })
        }

        const compile = Compiler.init(callback)

        app.ports.compileOnClientOut.subscribe(function ([html, elm, packages, forSave]) {
          runForSave = forSave
          compile({ html, elm, packages, forSave })
        })
      })


  })
