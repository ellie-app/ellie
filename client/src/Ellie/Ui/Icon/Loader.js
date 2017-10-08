const load = () => {
  return import(/* webpackChunkName: "icons-svg" */ './icons.svg')
    .then((text) => {
      const hack = document && document.documentElement.doScroll
      let loaded = document && (hack ? /^loaded|^c/ : /^loaded|^i|^c/).test(document.readyState)
      const callback = () => {
        const div = document.createElement('div')
        div.style.display = 'none'
        div.innerHTML = text
        document.body.insertBefore(div, document.body.childNodes[0])
        document.removeEventListener('DOMContentLoaded', callback)
        loaded = true
      }

      if (!loaded) {
        document.addEventListener('DOMContentLoaded', callback)
      } else {
        setTimeout(callback, 0)
      }
    })
}

export default {
  load
}
