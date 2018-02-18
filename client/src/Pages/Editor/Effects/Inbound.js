const start = (app) => {
  // Patch WebSocket to send close events through our port
  // Bummer that the WebSocket module doesn't do this for us
  // but whatchagonnadoamiright
  window.WebSocket = class extends WebSocket {
    constructor(...args) {
      super(...args)
      this.addEventListener('close', () => {
        app.ports.pagesEditorEffectsInbound.send({
          tag: 'SocketClosed',
          data: this.url
        })
      })
    }
  }
}

export default { start }
