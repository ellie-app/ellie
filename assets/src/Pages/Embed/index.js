import EllieUiIcon from "../../Ellie/Ui/Icon";
import EllieUiCodeEditor from "../../Ellie/Ui/CodeEditor";
import EllieUiOutput from "../../Ellie/Ui/Output";
import EllieConstants from "../../Ellie/Constants";
import NetworkAbsintheSocket from "../../Network/Absinthe/Socket";
import PagesEmbedMain from "./Main";
import "../../Ellie/Ui/CodeEditor.css";
import "../../Ellie/Ui/Logo.css";
import { Elm } from "./Main.elm";

__webpack_public_path__ = EllieConstants.publicPath();

document.addEventListener("DOMContentLoaded", () => {
  let flags = {};

  const app = Elm.Pages.Embed.Main.init({ flags });

  NetworkAbsintheSocket.start(app);
  EllieUiOutput.start(app);
  EllieUiCodeEditor.start(app);
  EllieUiIcon.start(app);
  PagesEmbedMain.start(app);
});
