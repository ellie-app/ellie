const path =
  process.env.NODE_ENV === 'production' ?
    `${CDN_BASE}/images/icons.svg` :
    'http://localhost:8000/images/icons.svg'

const load = () => {
  const xhr =
    new XMLHttpRequest()

  xhr.addEventListener('load', () => {
    const hack = document && document.documentElement.doScroll
    let loaded = document && (hack ? /^loaded|^c/ : /^loaded|^i|^c/).test(document.readyState)
    const callback = () => {
      const div = document.createElement('div')
      div.style.display = 'none'
      div.innerHTML = xhr.responseText
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

  xhr.open('GET', path)
  xhr.send()
}

export default {
  load
}
