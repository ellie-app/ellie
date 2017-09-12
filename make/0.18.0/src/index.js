const Elm = require('./Main.elm')
const Compiler = require('./Worker/ConcurrentCompiler')

export const init = listener => {
  let requestId = 0
  const worker = Elm.Main.worker()

  const compilerPromise =
    Compiler
      .init(
        CDN_BASE + '/elm-compilers/0.18.0-dev.js',
        percentage => worker.ports.msgsIn.send({ type: 'LoadedMoreCode', args: [percentage] })
      )
      .then(ElmCompiler => self.ElmCompiler = ElmCompiler)

  const waitForCompiler = (cb) => {
    if (self.ElmCompiler) cb()
    else compilerPromise.then(() => cb())
  }

  worker.ports.stageChangedOut.subscribe(listener)

  worker.ports.compileOut.subscribe(([buildNumber, scriptFile]) => {
    listener({ type: 'Success', url: scriptFile })
  });

  const clearElmStuff = () => {
    worker.ports.msgsIn.send({ type: 'ClearElmStuff' })
  }

  const compile = ({ packages, elm }) => {
    requestId += 1

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
        args: [requestId, desc, elm]
      })
    })

    return requestId
  }

  return { clearElmStuff, compile }
}
