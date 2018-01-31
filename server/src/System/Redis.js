exports._createClient = function _createClient(options) {
  return function _createClientEff() {
    var Redis = require('redis-fast-driver')
    return new Redis(options)
  }
}

exports._get = function _get(nothing, just, inputs) {
  return function _getAff(fail, succeed) {
    var client = inputs.client
    var key = inputs.key
    client.rawCall(['get', key], function (error, response) {
      if (error) fail(error)
      else if (response == null) succeed(nothing)
      else succeed(just(JSON.parse(response)))
    })
  }
}

exports._set = function _set(inputs) {
  return function _setAff(fail, succeed) {
    var client = inputs.client
    var key = inputs.key
    var value = inputs.value
    var unit = inputs.unit
    client.rawCall(['set', key, JSON.stringify(value)], function (error, response) {
      if (error) fail(error)
      else succeed(unit)
    })
  }
}

exports._exists = function _exists(inputs) {
  return function _existsAff(fail, succeed) {
    var client = inputs.client
    var key = inputs.key
    client.rawCall(['exists', key], function (error, response) {
      if (error) fail(error)
      else succeed(response === 1)
    })
  }
}