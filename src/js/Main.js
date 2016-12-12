require('../index.html')
require('./Main.css')
require('../elm/Stylesheets.elm')
const initCodeMirror = require('./CodeMirror')

initCodeMirror()
  .then(() => {
    const Elm = require('../elm/Main.elm')
    const app = Elm.Main.fullscreen()

    window.addEventListener('beforeunload', () => {
      app.ports.windowUnloadedIn.send([])
    })

    window.addEventListener('message', (event) => {
      app.ports.windowMessageIn.send(event.data)
    })
  })
