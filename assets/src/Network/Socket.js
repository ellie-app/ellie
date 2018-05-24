var OldWebSocket = window.WebSocket

window.WebSocket = function (url, protocols) {
  if (url.indexOf('ELM_LANG_SOCKET::') !== 0) {
    return new OldWebSocket(url, protocols)
  }

  var ws = new OldWebSocket(url.replace('ELM_LANG_SOCKET::', ''), protocols)
  var oldAddEventListener = OldWebSocket.prototype.addEventListener
  ws.addEventListener = function(type, callback) {
    if (type === 'message') {
      oldAddEventListener.call(this, type, (event) => {
        if (event.isTrusted) {
          Object.defineProperty(event, 'data', { value: JSON.stringify({ type: 'Data', data: JSON.parse(event.data) }) })
        }
        callback(event)
      })
      return
    }
    
    if (type === 'open') {
      oldAddEventListener.call(this, type, (event) => {
        callback(event)
        setTimeout(() => {
          this.dispatchEvent(new MessageEvent('message', { data: JSON.stringify({ type: 'Open' })}))
        })
      })
    }

    if (type === 'close') {
      oldAddEventListener.call(this, type, (event) => {
        this.dispatchEvent(new MessageEvent('message', { data: JSON.stringify({ type: 'Close' })}))
        setTimeout(() => {
          callback(event)
        })
      })
      return
    }

    oldAddEventListener.call(this, type, callback)
  }

  // this might have to change if we have multiple subscriptions
  var oldRemoveEventListener = OldWebSocket.prototype.removeEventListener
  ws.removeEventListener = function(type, callback) {
    return oldRemoveEventListener.call(type)
  }
  return ws
}

export default {
  start(app) {
    
  }
}
