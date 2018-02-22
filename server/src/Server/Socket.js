function noop () {}

function heartbeat() {
  this.isAlive = true
}

function close() {
  this.pursConfig.onDisconnect(this.pursIdentity)
}

function makeSend(socket) {
  return function (data) {
    if (socket.readyState === 1) {
      socket.send(data)
    }
    return socket.pursHelpers.unit
  }
}

function message(data) {
  this.pursConfig.onMessage(this.pursIdentity, data, makeSend(this))
}

exports._listen = function _listen(unit, server, config) {
  var WebSocket = require('ws')
  var wss = new WebSocket.Server({
    server: server,
    path: config.path,
    verifyClient: function (info, callback) {
      config.onAuthenticate(info.req, function (identity) {
        if (typeof identity === 'undefined') {
          callback(false)
        } else {
          info.req.pursIdentity = identity
          callback(true)
        }
      })
    }
  })

  wss.on('error', function (error) {
    if (error.code === 'ECONNRESET') return
    throw error
  })

  wss.on('connection', function (ws, req) {
    ws.on('error', function (error) {
      if (error.code === 'ECONNRESET') return
      throw error
    })
    ws.pursIdentity = req.pursIdentity
    ws.pursHelpers = { unit: unit }
    ws.pursConfig = config
    ws.isAlive = true
    ws.on('pong', heartbeat)
    ws.on('close', close)
    ws.on('message', message)
    config.onConnect(ws.pursIdentity, makeSend(ws))
  })

  setInterval(function () {
    wss.clients.forEach(function each(ws) {
      if (ws.isAlive === false) return ws.terminate()
      ws.isAlive = false
      ws.ping(noop)
    })
  }, 30000)

  return wss
}
