const throttleToFrame = (fn) => {
  let scheduled = false
  let args = null
  return function () {
    args = Array.from(arguments)
    if (scheduled) return
    scheduled = true
    requestAnimationFrame(() => {
      scheduled = false
      fn.apply(null, args)
    })
  }
}

customElements.define('ellie-ui-split-pane-group', class extends HTMLElement {
  constructor() {
    super()
    this.onDocumentMouseMove = this.onDocumentMouseMove.bind(this)
    this.updatePosition = throttleToFrame(this.updatePosition.bind(this))
    this.onDocumentMouseUp = this.onDocumentMouseUp.bind(this)
    this.onDividerMouseDown = this.onDividerMouseDown.bind(this)
    this._scheduled = false
    this._dragging = false
    this._horizontal = true
  }

  onDocumentMouseMove(event) {
    this.updatePosition(event.pageX)
  }

  updatePosition(pageX) {
    const rect = this.getBoundingClientRect()
    const ratio = (pageX - rect.left) / rect.width
    this.children[0].style.width = this.children[1].style.left = `${ratio * 100}%`
    this.children[2].style.width = `${(1 - ratio) * 100}%`
  }

  onDocumentMouseUp() {
    this.style.cursor = null
    document.removeEventListener('mousemove', this.onDocumentMouseMove)
    document.removeEventListener('mouseup', this.onDocumentMouseUp)
  }

  onDividerMouseDown() {
    this.style.cursor = 'ew-resize'
    document.addEventListener('mousemove', this.onDocumentMouseMove)
    document.addEventListener('mouseup', this.onDocumentMouseUp)
  }

  connectedCallback() {
    this.children[1].addEventListener('mousedown', this.onDividerMouseDown)
  }

  disconnectedCallback() {
    document.removeEventListener('mousemove', this.onDocumentMouseMove)
    document.removeEventListener('mouseup', this.onDocumentMouseUp)
  }
})