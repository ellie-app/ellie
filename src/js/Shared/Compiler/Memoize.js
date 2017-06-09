function defaultSerialize() {
  return JSON.stringify(arguments)
}

export function memoize(fn, serializeArguments) {
  var cache = Object.create(null)

  var serialize = typeof serializeArguments === 'function' ?
    serializeArguments :
    defaultSerialize

  return function () {
    var cacheKey = serialize.apply(null, arguments)

    if (!(cacheKey in cache)) {
      var computedValue = fn.apply(null, arguments)
      cache[cacheKey] = computedValue
      return computedValue
    }

    return cache[cacheKey]
  }
}

function defaultSerializeAsync() {
  return Promise.resolve(JSON.stringify(arguments))
}

export function memoizeAsync(fn, serializeArguments) {
  var cache = Object.create(null)

  var serialize = typeof serializeArguments === 'function' ?
    serializeArguments :
    defaultSerializeAsync

  return function () {
    var args = arguments
    var cacheKey = serialize.apply(null, args)

    if (!(cacheKey in cache)) {
      return Promise.resolve()
        .then(function () {
          return fn.apply(null, args)
        })
        .then(function (result) {
          cache[cacheKey] = result
          return result
        })
    }

    return Promise.resolve(cache[cacheKey])
  }
}
