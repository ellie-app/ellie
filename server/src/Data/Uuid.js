var validate = require('uuid-validate')
var Encoder = require('uuid-encoder')
var encoder = new Encoder('23456789bcdfghjkmnpqrstvwxyzBCDFGHJKLMNPQRSTVWXYZ')

exports._encode = function _encode(uuid) {
  return encoder.encode(uuid) + 'a2'
}

exports._decode = function _decode(inputs) {
  try {
    return inputs.just(encoder.decode(inputs.string.replace('a2', '')))
  } catch (e) {
    return inputs.nothing
  }
}

exports._validate = function _validate(string) {
  return validate(string)
}
