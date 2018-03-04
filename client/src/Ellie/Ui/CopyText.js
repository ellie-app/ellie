customElements.define('ellie-ui-copy-text', class extends HTMLElement {
  constructor() {
    super()
    this._timeout = null
    this._onButtonClick = this._onButtonClick.bind(this)
    this._onContentClick = this._onContentClick.bind(this)
  }

  connectedCallback() {
    this._button = this.querySelector('button')
    this._content = this.querySelector('ellie-ui-copy-text-content')
    this._note = this.querySelector('ellie-ui-copy-text-note')
    this._button.addEventListener('click', this._onButtonClick)
    this._content.addEventListener('click', this._onContentClick)
  }

  disconnectedCallback() {
    clearTimeout(this._timeout)
  }

  _onContentClick() {
    const selection = window.getSelection()
    const range = document.createRange()
    range.selectNodeContents(this._content)
    selection.removeAllRanges()
    selection.addRange(range)
  }

  _onButtonClick() {
    const selection = window.getSelection()
    const range = document.createRange()
    range.selectNodeContents(this._content)
    selection.removeAllRanges()
    selection.addRange(range)
    let successful = false
    try {
      successful = document.execCommand('copy')
    } catch (error) {
      successful = false
    }

    if (successful) {
      selection.removeAllRanges()
      this._note.textContent = 'Copied to Clipboard'
    } else {
      this._note.textContent = '⌘C to Copy'
    }
    clearTimeout(this._timeout)
    this._timeout = setTimeout(() => {
      this._note.style.visibility = 'hidden'
    }, 3000)
    this._note.style.visibility = 'visible'
  }
})