exports._read = function _read(path) {
  return function (fail, succeed) {
    var fs = require('fs')
    fs.readFile(path, function (error, data) {
      if (error) fail(error)
      else succeed(data.toString())
    })
  }
}

exports._write = function _write(unit, path, data) {
  return function (fail, succeed) {
    var fs = require('fs')
    fs.writeFile(path, data, function (error) {
      if (error) fail(error)
      else succeed(unit)
    })
  }
}

exports._exists = function _exists(path) {
  return function (fail, succeed) {
    var fs = require('fs')
    fs.exists(path, succeed)
  }
}

exports._createDirectory = function _createDirectory(unit, path) {
  return function (fail, succeed) {
    var mkdirp = require('mkdirp')
    mkdirp(path, function (error) {
      if (error) fail(error)
      else succeed(unit)
    })
  }
}