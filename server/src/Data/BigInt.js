exports._add = function _add(n) {
  return function (m) { return n.add(m) }
}

exports._subtract = function _sub(n) {
  return function (m) { return n.sub(m) }
}

exports._multiply = function _mul(n) {
  return function (m) { return n.mul(n) }
}

exports._divide = function _div(n) {
  return function (m) { return n.div(m) }
}

exports._compare = function _compare(n) {
  return function (m) { return n.cmp(m) }
}

exports._eq = function _eq(n) {
  return function (m) { return n.eq(m) }
}

exports._mod = function _mod(n) {
  return function (m) { return n.mod(m) }
}

exports._degree = function (n) {
  var min = 2147483647
  var abs = n.abs()
  if (abs.lt(min)) return abs.toNumber()
  else return min
}

exports._fromInt = function _fromInt(n) {
  var bignum = require('bignum')
  return bignum(n)
}

exports._toInt = function _toInt(n) {
  if (n.gt(Number.MAX_SAFE_INTEGER)) return Number.MAX_SAFE_INTEGER
  else if (n.lt(-Number.MAX_SAFE_INTEGER)) return -Number.MAX_SAFE_INTEGER
  else return n.toNumber()
}

exports._shiftLeftBy = function _shiftLeftBy(by) {
  return function (n) { return n.shiftLeft(by) }
}

exports._orWith = function _orWith(n) {
  return function (m) { return m.or(n) }
}