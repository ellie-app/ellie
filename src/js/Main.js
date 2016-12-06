require('../index.html')
require('muicss/dist/css/mui.css')
const initCodeMirror = require('./CodeMirror')

initCodeMirror()
  .then(() => {
    const Elm = require('../elm/Main.elm')
    const app = Elm.Main.fullscreen()    
  })
