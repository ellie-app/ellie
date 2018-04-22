const start = (app) => {
  app.ports.pagesEmbedEffectsStateOut.subscribe(({ tag, contents }) => {
    switch (tag) {
      case 'GoToPosition':
        requestAnimationFrame(() => {
          const editor = document.querySelector('code-editor')
          if (!editor) return
          const [line, column] = contents
          editor.moveCursor(line, column)
        })
        break

      default:
        break
    }
  })
}

export default { start }
