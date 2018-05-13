import CustomElements from '../../Platform/CustomElements'

export default {
  start(app) {
    CustomElements.define('ellie-ui-menu', (HTMLElement) => class extends HTMLElement {
      constructor() {
        super()
        this._isOpen = false
        this._toggle = this._toggle.bind(this)
        this._onDocumentClick = this._onDocumentClick.bind(this)
        this._close = this._close.bind(this)
      }

      connectedCallback() {
        this._toggleButton = this.querySelector('[data-ellie-ui-menu-toggle]')
        this._items = this.querySelector('ellie-ui-menu-items')
        this._toggleButton.addEventListener('click', this._toggle)
        document.addEventListener('click', this._onDocumentClick)
      }

      disconnectedCallback() {
        document.removeEventListener('click', this._onDocumentClick)
      }

      _onDocumentClick(event) {
        if (!this._isOpen) return
        if (event.target === this._toggleButton) return
        if (this.contains(event.target)) {
          if (event.target.tagName === 'BUTTON' || event.target.tagName === 'A') this._close()
          return
        }
        this._close()
      }

      _close() {
        if (!this._isOpen) return
        this._isOpen = false
        this._items.style.display = 'none'
      }

      _toggle() {
        this._items.style.display = this._isOpen ? 'none' : 'block'
        this._isOpen = !this._isOpen
      }
    })
  }
}
