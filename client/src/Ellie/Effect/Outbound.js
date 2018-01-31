const start = (app) => {
  app.ports.ellieEffectsOutbound.subscribe(({ tag, contents }) => {
    console.log(tag)
    switch (tag) {
      case 'SaveToken':
        const [token] = contents
        localStorage.setItem('Pages.Editor.token', token)
        break

      case 'ReloadIframe':
        const [iframeId] = contents
        const iframe = document.getElementById(iframeId)
        if (iframe) iframe.src = iframe.src
        break

      default:
        break
    }
  })
}

export default { start }