import 'es6-promise/auto'
import './Main.css'
import '../../Ellie/Ui/SplitPane'
import '../../Ellie/Ui/Icon'
import '../../Ellie/Ui/Menu'
import CodeEditor from '../../Ellie/Ui/CodeEditor'
import OpbeatRunner from '../../Ellie/Opbeat'
import Outbound from './Effects/Outbound'
import Inbound from './Effects/Inbound'
import './Views/Setup.css'
import './Views/Output'


CodeEditor
  .initialize()
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

    const acceptedTermsVersion = JSON.parse(document.querySelector('meta[name=accepted_terms_version]').content)
    const latestTermsVersion = JSON.parse(document.querySelector('meta[name=latest_terms_version]').content)

    const app = Elm.Pages.Editor.Main.fullscreen({
      token: localStorage.getItem('Pages.Editor.token')
    })

    Inbound.start(app)
    Outbound.start(app)

    // OpbeatRunner.start(app)

    // app.ports.pathChangedOut.subscribe(() => {
    //   previousLocation = window.location.pathname
    // })

    // app.ports.hasUnsavedWork.subscribe(nextValue => {
    //   hasUnsavedWork = nextValue
    // })

    // app.ports.reloadIframeOut.subscribe(() => {
    //   var iframe = document.getElementById('results_iframe')
    //   if (!iframe) return
    //   iframe.src = iframe.src
    // })

    // app.ports.openDebuggerOut.subscribe(() => {
    //   var iframe = document.getElementById('results_iframe')
    //   if (!iframe) return
    //   iframe.contentWindow.postMessage({ type: 'debug' }, API_ORIGIN)
    // })

    // app.ports.openNewWindow.subscribe(url => {
    //   var win = window.open(url, '_blank')
    //   win.focus()
    // })

    // app.ports.saveVimMode.subscribe(enabled => {
    //   localStorage.setItem('Pages.Editor.vimMode', enabled)
    // })


    // window.addEventListener('beforeunload', function (e) {
    //   if (hasUnsavedWork) {
    //     e.returnValue = 'You have unsaved work. Are you sure you want to go?'
    //   }
    // })

    // window.addEventListener('message', function (event) {
    //   if (event.origin !== SERVER_ORIGIN) return
    //   if (event.data.type === 'error') {
    //     app.ports.jsError.send(event.data.message)
    //   }
    // })
  })
