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
        const iframeId = contents
        const iframe = document.getElementById(iframeId)
        if (iframe) iframe.src = iframe.src
        break

      case 'EnableNavigationCheck':
        const enabled = contents
        if (enabled) window.addEventListener('beforeunload', preventNavigation)
        else window.removeEventListener('beforeunload', preventNavigation)
        break
      
      case 'SwitchToDebugger':
        requestAnimationFrame(() => {
          const iframe = document.getElementById('workbenchIframe')
          if (!iframe) return
          iframe.contentWindow.postMessage({ tag: 'SwitchToDebugger' }, window.location.origin)
        })
        break

      case 'SwitchToProgram':
        requestAnimationFrame(() => {
          const iframe = document.getElementById('workbenchIframe')
          if (!iframe) return
          iframe.contentWindow.postMessage({ tag: 'SwitchToProgram' }, window.location.origin)
        })
        break

      default:
        break
    }
  })
}

export default { start }
