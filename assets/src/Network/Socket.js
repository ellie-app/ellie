import Msgpack from 'msgpack-lite'

export default {
  start(app) {
    window.WebSocket = class extends WebSocket {
      constructor(url, protocols) {
        if (url.indexOf('ELM_LANG_SOCKET::') === 0) {
          super(url.replace('ELM_LANG_SOCKET::', ''), protocols)
          this._isElm = true
          this.binaryType = 'arraybuffer'
        } else {
          super(url, protocols)
        }
      }

      send(data) {
        if (!this._isElm) {
          return super.send(data)
        }
        return super.send(Msgpack.encode(JSON.parse(data)))
      }

      addEventListener(type, callback) {
        if (!this._isElm) {
          super.addEventListener(type, callback)
          return
        }

        if (type === 'message') {
          super.addEventListener(type, (event) => {
            if (event.isTrusted) {
              const data = Msgpack.decode(new Uint8Array(event.data))
              Object.defineProperty(event, 'data', { value: JSON.stringify({ type: 'Data', data }) })
            }
            callback(event)
          })
          return
        }
        
        if (type === 'open') {
          super.addEventListener(type, (event) => {
            callback(event)
            setTimeout(() => {
              this.dispatchEvent(new MessageEvent('message', { data: JSON.stringify({ type: 'Open' })}))
            })
          })
        }

        if (type === 'close') {
          super.addEventListener(type, (event) => {
            this.dispatchEvent(new MessageEvent('message', { data: JSON.stringify({ type: 'Close' })}))
            setTimeout(() => {
              callback(event)
            })
          })
        }
      }

      removeEventListener(type, callback) {
        if (this._isElm) super.removeEventListener(type)
        else super.removeEventListener(type, callback)
      }
    }
  }
}
