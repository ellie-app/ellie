let compiler

self.addEventListener('message', event => {
  if (event.data.type === 'load') {
    const [scriptUrl] = event.data.args
    importScripts(scriptUrl)
    compiler = require('Dll/ElmCompiler')()
    self.postMessage({ type: 'load' })
  } else if (event.data.type === 'ready') {
    self.postMessage({ type: 'ready' })
  } else if (event.data.type === 'compile') {
    compiler.compile(...event.data.args)
      .then(result => {
        self.postMessage({ id: event.data.id, type: 'compile', success: true, result })
      })
      .catch(error => {
        self.postMessage({ id: event.data.id, type: 'compile', success: false, message: error.message })
      })
  } else if (event.data.type === 'parse') {
    compiler.parse(...event.data.args)
      .then(result => {
        self.postMessage({ id: event.data.id, type: 'parse', success: true, result })
      })
      .catch(error => {
        self.postMessage({ id: event.data.id, type: 'parse', success: false, message: error.message })
      })
  }
})

self.postMessage({ type: 'ready' })
