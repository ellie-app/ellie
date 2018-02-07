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
    this._dragging = false
    this._isVertical = false
    this._ratio = 0.5
    this._defaultRatio = 0.5
  }

  get isVertical() {
    return this._isVertical
  }

  set isVertical(value) {
    this._isVertical = value
    if (this.childElementCount) this.reflow()
    if (this._dragging) this.style.cursor = value ? 'ns-resize' : 'ew-resize'
  }

  get defaultRatio() {
    return this._defaultRatio
  }

  set defaultRatio(value) {
    this._defaultRatio = value
  }

  onDocumentMouseMove(event) {
    this.updatePosition(event.pageX, event.pageY)
  }

  updatePosition(pageX, pageY) {
    const rect = this.getBoundingClientRect()
    this._ratio = this._isVertical ?
       (pageY - rect.top) / rect.height :
       (pageX - rect.left) / rect.width
    if (this.childElementCount) this.reflow()
  }

  reflow() {
    if (this._isVertical) {
      this.children[0].style.height = this.children[1].style.top = `${this._ratio * 100}%`
      this.children[2].style.height = `${(1 - this._ratio) * 100}%`
    } else {
      this.children[0].style.width = this.children[1].style.left = `${this._ratio * 100}%`
      this.children[2].style.width = `${(1 - this._ratio) * 100}%`
    }
  }

  onDocumentMouseUp() {
    this.style.cursor = null
    this._dragging = false
    document.removeEventListener('mousemove', this.onDocumentMouseMove)
    document.removeEventListener('mouseup', this.onDocumentMouseUp)
  }

  onDividerMouseDown() {
    this.style.cursor = this._isVertical ? 'ns-resize' : 'ew-resize'
    this._dragging = true
    document.addEventListener('mousemove', this.onDocumentMouseMove)
    document.addEventListener('mouseup', this.onDocumentMouseUp)
  }

  connectedCallback() {
    this.children[1].addEventListener('mousedown', this.onDividerMouseDown)
    this._ratio = this._defaultRatio
    this.reflow()
  }

  disconnectedCallback() {
    document.removeEventListener('mousemove', this.onDocumentMouseMove)
    document.removeEventListener('mouseup', this.onDocumentMouseUp)
  }
})