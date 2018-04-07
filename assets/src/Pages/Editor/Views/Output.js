export default {
  start(app) {
    const persistentIframe = document.createElement('iframe')

    customElements.define('ellie-pages-editor-views-output', class extends HTMLElement {
      constructor() {
        super()
        this._src = null
        this._debug = false
        this._attached = false
        this._onLoad = this._onLoad.bind(this)
        this._onMessage = this._onMessage.bind(this)
      }

      get src() {
        return this._src
      }

      set src(value) {
        this._src = value
        if (!this._attached) return
        persistentIframe.src = value
      }

      get debug() {
        return this._debug
      }

      set debug(value) {
        this._debug = value
        if (!this._attached) return
        if (value) {
          persistentIframe.contentWindow.postMessage({ tag: 'SwitchToDebugger' }, window.location.origin)
        } else {
          persistentIframe.contentWindow.postMessage({ tag: 'SwitchToProgram' }, window.location.origin)
        }
      }

      connectedCallback() {
        this.appendChild(persistentIframe)
        this._attached = true
        window.addEventListener('message', this._onMessage)
        persistentIframe.addEventListener('load', this._onLoad)
        if (!persistentIframe.src || persistentIframe.src.indexOf(this._src) === -1) {
          persistentIframe.src = this._src
        }
      }

      disconnectedCallback() {
        window.removeEventListener('message', this._onMessage)
        persistentIframe.removeEventListener('load', this._onLoad)
        const portal = document.querySelector('ellie-ui-portal')
        portal.appendChild(persistentIframe)
        this._attached = false
      }

      reload() {
        if (!this._attached) return
        persistentIframe.src = persistentIframe.src
      }

      _onMessage(e) {
        switch (e.data.tag) {
          case 'LogReceived':
            this.dispatchEvent(new CustomEvent('log', { detail: e.data.contents }))
        }
      }

      _onLoad() {
        if (this._debug) {
          persistentIframe.contentWindow.postMessage({ tag: 'SwitchToDebugger' }, window.location.origin)
        } else {
          persistentIframe.contentWindow.postMessage({ tag: 'SwitchToProgram' }, window.location.origin)
        }
      }
    })
  }
}
