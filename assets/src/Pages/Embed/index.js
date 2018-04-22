import 'es6-promise/auto'
import EllieUiIcon from '../../Ellie/Ui/Icon'
import EllieUiCodeEditor from '../../Ellie/Ui/CodeEditor'
import EllieUiOutput from '../../Ellie/Ui/Output'
import NetworkSocket from '../../Network/Socket'
import PagesEmbedEffectsState from './Effects/State'
import '../../Ellie/Ui/CodeEditor.css'
import '../../Ellie/Ui/Logo.css'
import { Pages } from './Main.elm'

document.addEventListener('DOMContentLoaded', () => {
  let flags = {}
  const app = Pages.Embed.Main.fullscreen(flags)
  PagesEmbedEffectsState.start(app)
  NetworkSocket.start(app)
  EllieUiOutput.start(app)
  EllieUiCodeEditor.start(app)
  EllieUiIcon.start(app)
})
