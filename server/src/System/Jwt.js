var jwt = require('jsonwebtoken')

exports._encode = function _encode(inputs) {
  var input = inputs.input
  var secret = inputs.secret
  return function (fail, succeed) {
    jwt.sign(input, secret, function(error, token) {
      if (error) fail(error)
      else succeed(token)
    })
  }
}

exports._decode = function _decode(inputs) {
  var input = inputs.input
  var secret = inputs.secret
  return function (fail, succeed) {
    jwt.verify(input, secret, function (error, data) {
      if (error) fail(error)
      else succeed(data)
    })
  }
}