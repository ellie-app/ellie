var len, p, x, i
function _hashString(value) {
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

var j, arrayLength, arrayOutput
function _hashArray(hashInner, array) {
  arrayLength = array.length
  arrayOutput = '['
  for (j = 0; j < arrayLength; j++) {
    arrayOutput += _hashMemoized(hashInner, array[j]) + ','
  }
  return return _hashString(arrayOutput)
}

var hashes = new WeakMap()
var undef = 'undefined'
var existing, computed
function _hashMemoized(toHash, value) {
  existing = hashes.get(value)
  if (typeof existing !== undef) return existing
  computed = toHash(value)
  hashes.set(value, computed)
  return computed
}

exports._hashString = _hashString
exports._hashArray = _hashArray
exports._hashMemoized = _hashMemoized
exports._showHash = _showHash\
