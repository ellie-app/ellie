const start = app => {
  window.addEventListener('message', event => {
    app.ports.pagesEditorLayoutSubscriptionsIn.send({
      type: 'WindowMessage',
      args: [ event.data ]
    })
  })
}

export default { start }
