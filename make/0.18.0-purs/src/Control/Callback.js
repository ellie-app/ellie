exports._runCallback = function _runCallback(unit, callback, value) {
  return function () {
    callback(value)
    return unit
  }
}