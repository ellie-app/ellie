const start = app => {
  window.addEventListener('message', event => {
    if (event.origin !== SERVER_ORIGIN) return
    app.ports.pagesEditorLayoutSubscriptionsIn.send({
      type: 'WindowMessage',
      args: [ event.data ]
    })
  })
}

export default { start }
