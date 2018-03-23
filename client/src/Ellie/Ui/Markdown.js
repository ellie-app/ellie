const start = (_app) => {
  customElements.define('ellie-ui-markdown', class extends HTMLElement {
    constructor() {
      super()
      this._markdownValue = ''
      this._marked = null
    }

    // PROPERTIES

    get markdownValue() {
      return this._markdownValue
    }
    set markdownValue(value) {
      if (this._markdownValue === value) return
      this._markdownValue = value
      if (!this._marked) return
      this._render()
    }

    // PUBLIC METHODS

    connectedCallback() {
      import('marked').then(marked => {
        this._marked = marked
        this._render()
      })
    }

    _render() {
      if (!this._marked) return
      this.innerHTML = this._marked(this._markdownValue)
    }
  })
}

export default { start }
