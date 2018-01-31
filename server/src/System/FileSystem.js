var fs = require('fs')
var mkdirp = require('mkdirp')
var rimraf = require('rimraf')
var unzip = require('unzip')
var tmp = require('tmp')

exports._read = function _read(path) {
  return function (fail, succeed) {
    fs.readFile(path, function (error, data) {
      if (error) fail(error)
      else succeed(data.toString())
    })
  }
}

exports._write = function _write(unit) {
  return function (path) {
    return function (data) {
      return function (fail, succeed) {
        fs.writeFile(path, data, function (error) {
          if (error) fail(error)
          else succeed(unit)
        })
      }
    }
  }
}

exports._exists = function _exists(path) {
  return function (fail, succeed) {
    fs.exists(path, succeed)
  }
}

exports._createDirectory = function _createDirectory(unit) {
  return function (path) {
    return function (fail, succeed) {
      mkdirp(path, function (error) {
        if (error) fail(error)
        else succeed(unit)
      })
    }
  }
}

exports._unzip = function _unzip(unit) {
  return function (from) {
    return function (to) {
      return function (fail, succeed) {
        fs.createReadStream(from)
          .pipe(unzip.Extract({ path: to }))
          .on('error', fail)
          .on('end', function () { succeed(unit) })
      }
    }
  }
}

exports._remove = function _remove(unit) {
  return function (path) {
    return function (fail, succeed) {
      rimraf(path, function (error) {
        if (error) fail(error)
        else succeed(unit)
      })
    }
  }
}

exports._cwd = function _cdw() {
  return process.cwd()
}

exports._readDirectory = function _readDirectory(path) {
  return function _readDirectoryAff(fail, succeed) {
    fs.readdir(path, function (error, entries) {
      if(error) fail(error)
      else succeed(entries)
    })
  }
}

exports._createTemporaryDirectory = function _createTemporaryDirectory(fail, succeed) {
  tmp.dir({ unsafeCleanup: true }, function (error, path) {
    if (error) fail(error)
    else succeed(path)
  })
}