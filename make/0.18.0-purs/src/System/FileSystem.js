var Store = require('./Store')

exports._doesFileExist = function (path) {
  return function aff(success, error) {
    Store
      .get(path)
      .then(function (value) { return value !== null })
      .then(success)
      .catch(error)
  }
}

exports._getModificationTime = function (Just, Nothing, path) {
  return function aff(success, error) {
    Store
      .get(path)
      .then(function (value) {
        if (value === null) return Nothing
        else return Just(value.modified)
      })
      .then(success)
      .catch(error)
  }
}

exports._read = function (path) {
  return function aff(success, error) {
    Store
      .get(path)
      .then(function (data) { return data === null ? null : data.value })
      .then(success)
      .catch(error)
  }
}
