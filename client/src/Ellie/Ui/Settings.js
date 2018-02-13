customElements.define('ellie-ui-settings-container', class extends HTMLElement {
  constructor() {
    super()
    this._onDocumentClick = this._onDocumentClick.bind(this)
    this._toggle = this._toggle.bind(this)
    this._isOpen = false
  }

  _close() {
    this._isOpen = false
    if (!this._actions) return
    this._actions.style.display = 'none'
  }

  _open() {
    this._isOpen = true
    if (!this._actions) return
    this._actions.style.display = null
  }

  _toggle() {
    if (this._isOpen) this._close()
    else this._open()
  }

  _onDocumentClick(event) {
    if (!this._isOpen) return
    if (this.contains(event.target)) {
      if (event.target.tagName === 'BUTTON') this._close()
      return
    }
    this._close()
  }

  connectedCallback() {
    this._handle = this.querySelector('ellie-ui-settings-handle')
    this._actions = this.querySelector('ellie-ui-settings-actions')
    document.addEventListener('click', this._onDocumentClick)
    this._handle.addEventListener('click', this._toggle)
    if (this._isOpen) this._open()
    else this._close()
  }

  disconnectedCallback() {
    document.removeEventListener('click', this._onDocumentClick)
  }
})