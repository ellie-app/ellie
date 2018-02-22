const murmurHash3 = require("murmurhash3js")

exports._hash = function hash(input) {
  return murmurHash3.x64.hash128(input)
}
