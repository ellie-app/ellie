/**
 * I want to generate this file eventually. This is roughly how I want it to work:
 * 1. Crawl the Elm dependency graph for a specified entry point module
 * 2. Get topological sort of imports, somehow
 * 2. Iterate each module name and see if there are corresponding auxillary code files
 *    - JavaScript files can expose `imports`, `flags`, and `start`. The `imports`
 *      function lets you do async imports so you can take advantage of Webpack's
 *      code splitting. The `flags` function lets you add values to the flags that
 *      will be passed to start the app. The `start` function takes the started app
 *      and does whatever it wants. Maybe it's making a custom element. Maybe it's
 *      setting up ports. Anything.
 *    - CSS files just load
 *    - SVG files load async and inject into the body
 * 3. Generate the index.js file and call the JavaScript interface functions in
 *    topological order of module imports.
 */

import NetworkSocket from '../../Network/Socket'
import EffectProgram from '../../Effect/Program'
import EllieUiIcon from '../../Ellie/Ui/Icon'
import EllieUiMenu from '../../Ellie/Ui/Menu'
import EllieUiCopyText from '../../Ellie/Ui/CopyText'
import EllieUiSplitPane from '../../Ellie/Ui/SplitPane'
import EllieUiCodeEditor from '../../Ellie/Ui/CodeEditor'
import EllieUiOutput from '../../Ellie/Ui/Output'
import '../../Ellie/Ui/CodeEditor.css'
import './Views/Setup.css'
import './Main.css'
import Main from './Main'

const Elm = require('./Main.elm')

document.addEventListener('DOMContentLoaded', () => {
  let flags = {}
  flags = Main.flags(flags)

  const app = Elm.Pages.Editor.Main.fullscreen(flags)

  NetworkSocket.start(app)
  EffectProgram.start(app)
  EllieUiIcon.start(app)
  EllieUiMenu.start(app)
  EllieUiCopyText.start(app)
  EllieUiSplitPane.start(app)
  EllieUiCodeEditor.start(app)
  EllieUiOutput.start(app)
  Main.start(app)
})

if(navigator.serviceWorker) {
  navigator.serviceWorker.register(window.ellieConfig.serviceWorkerPath, { scope: '/' })
}
