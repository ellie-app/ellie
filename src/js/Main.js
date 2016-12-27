require('../index.html')
require('./Main.css')
require('../elm/Stylesheets.elm')
const initCodeMirror = require('./CodeMirror')

initCodeMirror()
  .then(() => {
    const Elm = require('../elm/Main.elm')

    const app = Elm.Main.fullscreen({
      windowSize: {
        width: window.innerWidth,
        height: window.innerHeight
      },
      online: window.navigator.onLine
    })

    window.addEventListener('online',  () => {
      app.ports.online.send(true)
    })

    window.addEventListener('offline', () => {
      app.ports.online.send(false)
    })

    window.addEventListener('beforeunload', () => {
      app.ports.windowUnloadedIn.send(null)
    })

    window.addEventListener('message', (event) => {
      app.ports.windowMessageIn.send(event.data)
    })
  })
