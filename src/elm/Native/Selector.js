var _user$project$Native_Selector = (function () {
  var nothing = {}
  var eq = _elm_lang$core$Native_Utils.eq

  function memoize(fn) {
    var previousArg = nothing
    var previousResult = nothing
    return function memoized(arg) {
      if (eq(previousArg, arg)) {
        previousArg = arg
        return previousResult
      }

      previousArg = arg
      previousResult = fn(arg)
      return previousResult
    }
  }


  return {
    memoize: memoize,
  }
}())
