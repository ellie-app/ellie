const Elm = require('./Main.elm')
const Compiler = require('./Worker/ConcurrentCompiler')

export const init = listener => {
  const worker = Elm.Main.worker();

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

  worker.ports.compileOut.subscribe(([buildNumber, scriptFile]) => {
    listener({ type: 'Success', url: scriptFile })
  });

  return ({ packages, elm }) => {
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
        args: [0, desc, elm]
      })
    })
  }
}
