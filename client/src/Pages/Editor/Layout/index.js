const start = app => {
  window.addEventListener('message', event => {
    if (event.data.type === 'webpackWarnings') return
    app.ports.pagesEditorLayoutSubscriptionsIn.send({
      type: 'WindowMessage',
      args: [ event.data ]
    })
  })
}

export default { start }
