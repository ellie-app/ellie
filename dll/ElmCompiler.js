var IdbDirectStorage = require('./IdbDirectStorage')
var storage = new IdbDirectStorage('ElmCompiler')

module.exports = function () {
  return storage.initialize().then(function () {
    global.localStorage = storage
    return require('../../browser-elm-compiler')
  })
}
