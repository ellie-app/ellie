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
    this.onWindowResize = throttleToFrame(this.onWindowResize.bind(this))
    this.onDocumentMouseUp = this.onDocumentMouseUp.bind(this)
    this.onDividerMouseDown = this.onDividerMouseDown.bind(this)
    this.onDividerDoubleClick = this.onDividerDoubleClick.bind(this)
    this._dragging = false
    this._isVertical = false
    this._ratio = 0.5
    this._originalRatio = 0.5
    this._minSize = 0
  }

  get minSize() {
    return this._minSize
  }

  set minSize(value) {
    if (value === this._minSize) return
    this._minSize = value
    this.forceLayout()
  }

  get originalRatio() {
    return this._originalRatio
  }

  set originalRatio(value) {
    this._originalRatio = value
  }

  get isVertical() {
    return this._isVertical
  }

  set isVertical(value) {
    if (value === this._isVertical) return
    this._isVertical = value
    if (this.childElementCount) this.forceLayout()
    if (this._dragging) this.style.cursor = value ? 'ns-resize' : 'ew-resize'
  }

  get ratio() {
    return this._ratio
  }

  set ratio(value) {
    if (value === this._ratio) return
    this._ratio = value
    this.forceLayout()
  }

  onWindowResize() {
    this.forceLayout()
  }

  onDocumentMouseMove(event) {
    this.updatePosition(event.pageX, event.pageY)
  }

  forceLayout() {
    this.layout(this.getBoundingClientRect())
  }

  updatePosition(pageX, pageY) {
    const rect = this.getBoundingClientRect()
    let nextRatio = this._isVertical ?
       (pageY - rect.top - this._minSize - 2) / rect.height :
       (pageX - rect.left - this._minSize - 2) / rect.width
    if (nextRatio === this._ratio) return
    this._ratio = nextRatio
    this.layout(rect)
    this.dispatchEvent(new Event('resize'))
  }

  layout(rect) {
    if (!this.childElementCount) return
    if (this._isVertical) {
      const minSizeRatio = (this._minSize + 1) / rect.height
      const adjustedRatio = Math.min(1 - (2 * minSizeRatio), Math.max(this._ratio, 0))
      this.children[0].style.height = this.children[1].style.top = `${(adjustedRatio + minSizeRatio) * 100}%`
      this.children[2].style.height = `${(1 - (adjustedRatio + minSizeRatio)) * 100}%`
    } else {
      const minSizeRatio = (this._minSize + 1) / rect.width
      const adjustedRatio = Math.min(1 - (2 * minSizeRatio), Math.max(this._ratio, 0))
      this.children[0].style.width = this.children[1].style.left = `${(adjustedRatio + minSizeRatio) * 100}%`
      this.children[2].style.width = `${(1 - (adjustedRatio + minSizeRatio)) * 100}%`
    }
  }

  onDocumentMouseUp() {
    this.style.cursor = null
    this.style.userSelect = null
    this.children[0].style.pointerEvents = this.children[2].style.pointerEvents = null
    this._dragging = false
    document.removeEventListener('mousemove', this.onDocumentMouseMove)
    document.removeEventListener('mouseup', this.onDocumentMouseUp)
  }

  onDividerMouseDown() {
    this.style.cursor = this._isVertical ? 'ns-resize' : 'ew-resize'
    this.style.userSelect = 'none'
    this.children[0].style.pointerEvents = this.children[2].style.pointerEvents = 'none'
    this._dragging = true
    document.addEventListener('mousemove', this.onDocumentMouseMove)
    document.addEventListener('mouseup', this.onDocumentMouseUp)
  }

  onDividerDoubleClick() {
    if (this._ratio === this._originalRatio) return
    this.ratio = this._originalRatio
    this.dispatchEvent(new Event('resize'))
  }

  connectedCallback() {
    this.children[1].addEventListener('mousedown', this.onDividerMouseDown)
    this.children[1].addEventListener('dblclick', this.onDividerDoubleClick)
    window.addEventListener('resize', this.onWindowResize)
    this.forceLayout()
  }

  disconnectedCallback() {
    document.removeEventListener('mousemove', this.onDocumentMouseMove)
    document.removeEventListener('mouseup', this.onDocumentMouseUp)
    window.removeEventListener('resize', this.onWindowResize)
  }
})