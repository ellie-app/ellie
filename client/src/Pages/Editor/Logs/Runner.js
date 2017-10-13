const start = app => {
  window.addEventListener('message', function (event) {
    if (event.origin !== SERVER_ORIGIN) return
    if (event.data.type === 'log') {
      app.ports.pagesEditorLogsIn.send(...)
    }
  })
}

export default {
  start
}
