var _user$project$Native_Hash = (function () {

function stringHash(value) {
  var len, p, x, i
  len = value.length
  i = 0
  p = value.charCodeAt(i)
  x = p << 7
  while (--len >= 0) {
    x = (1000003*x) ^ value.charCodeAt(i++)
  }
  x = x ^ len
  return x
}

function assignHash(hash, object) {
  Object.defineProperty(object, '$__elm_hash__', {
    writable: false,
    enumerable: false,
    configurable: false,
    value: hash
  })
  return object.$__elm_hash__
}

function elmHash(value) {
  if (typeof value === 'string') {
    return stringHash('s' + value)
  } else if (typeof value === 'number' || typeof value === 'boolean') {
    return stringHash(value.toString())
  } else {
    if (typeof value.$__elm_hash__ !== 'undefined') return value.$__elm_hash__
    return assignHash(stringHash(_elm_lang$core$Native_Utils.toString(value)), value)
  }
}

return {
  hash: elmHash
}

}())
