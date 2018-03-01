const start = (app) => {
  const preventNavigation = (e) => {
    e.returnValue = 'You have unsaved work. Are you sure you want to go?'
  }

  app.ports.pagesEditorEffectsOutbound.subscribe(({ tag, contents }) => {
    switch (tag) {
      case 'SaveToken':
        const token = contents
        localStorage.setItem('Pages.Editor.token', token)
        break

      case 'ReloadIframe':
        requestAnimationFrame(() => {
          const output = document.querySelector('ellie-pages-editor-views-output')
          if (output) output.reload()
        })
        break

      case 'EnableNavigationCheck':
        const enabled = contents
        if (enabled) window.addEventListener('beforeunload', preventNavigation)
        else window.removeEventListener('beforeunload', preventNavigation)
        break

      default:
        break
    }
  })
}

export default { start }
