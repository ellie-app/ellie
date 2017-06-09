import * as Elm from '../../elm/Make/Main.elm'
import * as Compiler from './Compiler/ConcurrentCompiler'
import * as HtmlFixer from './Compiler/HtmlFixer'

export const init = listener => {
  const worker = Elm.Make.Main.worker();

  const compilerPromise =
    Compiler
      .init(
        CDN_BASE + '/elm-compilers/0.18.0.js',
        percentage => worker.ports.msgsIn.send({ type: 'LoadedMoreCode', args: [percentage] })
      )
      .then(ElmCompiler => self.ElmCompiler = ElmCompiler)

  const waitForCompiler = (cb) => {
    if (self.ElmCompiler) cb()
    else compilerPromise.then(() => cb())
  }

  worker.ports.stageChangedOut.subscribe(listener)

  let htmlCode
  let runForSave

  const readFile = (file) =>
    new Promise((resolve, reject) => {
      const fr = new FileReader()
      fr.addEventListener('load', () => {
        resolve(fr.result)
      })
      fr.addEventListener('error', () => {
        reject(fr.error)
      })
      fr.readAsText(file)
    })

  const getSourceScript = (scriptFile) => {
    if (runForSave) {
      return readFile(scriptFile)
        .then(data => '<script>' + data + '</script>')
    } else {
      return Promise.resolve(`<script src=${URL.createObjectURL(scriptFile)}></script>`)
    }
  }

  worker.ports.compileOut.subscribe(([buildNumber, scriptFile]) => {
    getSourceScript(scriptFile)
      .then(sourceScript =>
        HtmlFixer.fix({
          htmlCode,
          embedApi: !runForSave,
          sourceScript
        })
      )
      .then(url => {
        listener({ type: 'Success', url: url })
      })
      .catch(error => {
        listener({ type: 'Failed', message: error.message })
      })
  });

  return ({ packages, elm, html, forSave }) => {
    htmlCode = html
    runForSave = forSave

    const desc = {
      repository: 'http://github.com/user/project',
      version: '1.0.0',
      'elm-version': '0.18.0 <= v < 0.19.0',
      summary: '',
      license: 'MIT',
      'source-directories': ['src'],
      'exposed-modules': [],
      dependencies: packages,
      'native-modules': false
    }

    waitForCompiler(() => {
      worker.ports.msgsIn.send({
        type: 'StartCompile',
        args: [0, desc, elm, !runForSave]
      })
    })
  }
}
