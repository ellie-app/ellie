import builtin from 'newless'

let polyfillPromise = null

const setup = () => {
  if (polyfillPromise !== null) {
    return polyfillPromise
  }

  if (typeof window.customElements === 'undefined') {
    polyfillPromise = import('@webcomponents/custom-elements')
      .then(() => builtin(window.HTMLElement))
  } else {
    polyfillPromise = import('@webcomponents/custom-elements/src/native-shim')
      .then(() => {
        window.HTMLElement.prototype.constructor = window.HTMLElement
        return builtin(window.HTMLElement)
      })
  }

  return polyfillPromise
}

export default {
  define(name, factory) {
    return setup().then((BaseClass) => {
      window.customElements.define(name, factory(BaseClass))
    })
  }
}
