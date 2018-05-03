import EllieUiIcon from '../../Ellie/Ui/Icon'
import EllieUiCodeEditor from '../../Ellie/Ui/CodeEditor'
import EllieUiOutput from '../../Ellie/Ui/Output'
import NetworkSocket from '../../Network/Socket'
import EffectProgram from '../../Effect/Program'
import PagesEmbedMain from './Main'
import '../../Ellie/Ui/CodeEditor.css'
import '../../Ellie/Ui/Logo.css'
import { Pages } from './Main.elm'

document.addEventListener('DOMContentLoaded', () => {
  let flags = {}
  const app = Pages.Embed.Main.fullscreen(flags)
  NetworkSocket.start(app)
  EffectProgram.start(app)
  EllieUiOutput.start(app)
  EllieUiCodeEditor.start(app)
  EllieUiIcon.start(app)
  PagesEmbedMain.start(app)
})
