export default {
  start(app) {
    window.WebSocket = class extends WebSocket {
      constructor(url, protocols) {
        if (url.indexOf('ELM_LANG_SOCKET::') === 0) {
          super(url.replace('ELM_LANG_SOCKET::', ''), protocols)
          this._isElm = true
        } else {
          super(url, protocols)
        }
      }

      addEventListener(type, callback) {
        if (!this._isElm) {
          super.addEventListener(type, callback)
          return
        }

        if (type === 'message') {
          super.addEventListener(type, (event) => {
            if (event.isTrusted) {
              Object.defineProperty(event, 'data', { value: JSON.stringify({ type: 'Data', data: event.data }) })
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
