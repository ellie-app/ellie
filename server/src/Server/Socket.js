var identitySymbol = Symbol('identity')
var noState = {}
var OPEN = 'OPEN'
var PENDING = 'PENDING'
var CLOSED = 'CLOSED'
function noop() {}
function onPong() {
  this.__purs_connection.status = OPEN
}
function onClose() {
  this.__purs_connection.status = CLOSED
}
function onMessage(message) {
  var ws = this
  var connection = ws.__purs_connection
  if (connection.state === noState) return
  connection.update(connection.state, message, function (error, response) {
    if (error) throw error
    if (typeof response.message !== 'undefined') ws.send(JSON.stringify(response.message))
    connection.state = response.state
  })
}
function onError(error) {
  if (error.code === 'ECONNRESET') return
  throw error
}

exports._listen = function _listen(server, config) {
  var WebSocket = require('ws')
  var wss = new WebSocket.Server({
    server: server,
    path: config.path,
    verifyClient: function (info, callback) {
      config.authenticate(info.req, function (error, identity) {
        if (error) {
          callback(false, 500)
        } else if (identity) {
          info.req[identitySymbol] = identity
          callback(true)
        } else {
          callback(false, 403)
        }
      })
    }
  })

  wss.on('error', onError)

  var connections = Object.create(null)
  wss.on('connection', function (ws, req) {
    ws.on('error', onError)
    var id = req[identitySymbol].hash
    var auth = req[identitySymbol].value
    var connection
    if (id in connections) {
      connection = connections[id]
      connection.status = OPEN
      connection.ws = ws
      ws.__purs_connection = connection
      ws.on('pong', onPong)
      ws.on('close', onClose)
      ws.on('message', onMessage)
      config.acknowledge(connection.state, function (error, result) {
        if (error) throw error
        connection.state = result.state
        ws.send(JSON.stringify(result.message))
      })
    } else {
      connection = { ws: ws, update: config.update, state: noState, status: OPEN }
      connections[id] = connection
      ws.__purs_connection = connection
      config.setup(auth, function (error, state) {
        if (error) throw error
        connection.state = state
        ws.on('pong', onPong)
        ws.on('close', onClose)
        ws.on('message', onMessage)
        config.acknowledge(state, function (error, result) {
          if (error) throw error
          connection.state = result.state
          ws.send(JSON.stringify(result.message))
        })
      })
    }
  })

  setInterval(function () {
    var stats = { unknown: 0, broken: 0, closed: 0 }
    for (var id in connections) {
      var connection = connections[id]
      if (connection.status === OPEN) {
        stats.unknown++
        connection.status = PENDING
        connection.ws.ping(noop)
      } else if (connection.status === PENDING) {
        stats.broken++
        connection.status = CLOSED
        connection.ws.terminate()
      } else {
        stats.closed++
        delete connections[id]
        config.teardown(connection.state, function (error) {
          if (error) throw error
        })
      }
    }
  }, 30000)

  return wss
}