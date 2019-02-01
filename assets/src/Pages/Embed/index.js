import EllieUiIcon from "../../Ellie/Ui/Icon";
import EllieUiCodeEditor from "../../Ellie/Ui/CodeEditor";
import EllieUiOutput from "../../Ellie/Ui/Output";
import NetworkAbsintheSocket from "../../Network/Absinthe/Socket";
import EffectProgram from "../../Effect/Program";
import PagesEmbedMain from "./Main";
import "../../Ellie/Ui/CodeEditor.css";
import "../../Ellie/Ui/Logo.css";
import { Elm } from "./Main.elm";

document.addEventListener("DOMContentLoaded", () => {
  let flags = {};

  const app = Elm.Pages.Embed.Main.init({ flags });

  NetworkAbsintheSocket.start(app);
  EffectProgram.start(app);
  EllieUiOutput.start(app);
  EllieUiCodeEditor.start(app);
  EllieUiIcon.start(app);
  PagesEmbedMain.start(app);
});
