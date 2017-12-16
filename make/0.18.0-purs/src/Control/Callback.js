exports._runCallback = function _runCallback(ffiHelpers, callback, value) {
  return function runAff(fail, succeed) {
    try {
      callback(value)
      succeed(ffiHelpers.right(ffiHelpers.unit))
    } catch (e) {
      fail(e)
    }
  }
}