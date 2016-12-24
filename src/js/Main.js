require('../index.html')
require('./Main.css')
require('../elm/Stylesheets.elm')
const initCodeMirror = require('./CodeMirror')

initCodeMirror()
  .then(() => {
    const Elm = require('../elm/Main.elm')
    const app = Elm.Main.fullscreen()

    window.addEventListener('online',  () => {
      app.ports.online.send(true)
    })

    window.addEventListener('offline', () => {
      app.ports.online.send(false)
    })

    app.ports.online.send(window.navigator.onLine)

    // window.addEventListener('beforeunload', () => {
    //   app.ports.windowUnloadedIn.send(null)
    // })
    //
    // window.addEventListener('message', (event) => {
    //   app.ports.windowMessageIn.send(event.data)
    // })
  })
