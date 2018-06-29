export default {
  start(app) {
    // hack until 0.19
    const shouldPreventDefault = (e) => {
      return ((e.metaKey || e.ctrlKey) && e.key === 's')
        || ((e.metaKey || e.ctrlKey) && e.shiftKey && e.key === 'd')
        || ((e.metaKey || e.ctrlKey) && e.key === ',')
        || ((e.metaKey || e.ctrlKey) && e.shiftKey && e.key === 'r')
    }
    let queue = []
    let callback = null
    // window.addEventListener('keydown', (e) => {
    //   if (e.key === 'Meta' || e.key === 'Control' || e.key === 'Shift') return
    //   if (shouldPreventDefault(e)) e.preventDefault()
    //   queue.push(e)
    //   cancelIdleCallback(callback)
    //   callback = requestIdleCallback(() => {
    //     queue.forEach(app.ports.effectProgramKeyDowns.send)
    //     queue = []
    //   })
    // })

    app.ports.effectProgramTitle.subscribe((title) => {
      document.title = title
    })
  }
}
