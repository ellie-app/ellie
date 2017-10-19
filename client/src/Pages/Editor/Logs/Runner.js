const start = app => {
  window.addEventListener('message', function (event) {
    if (event.origin !== SERVER_ORIGIN) return
    if (event.data.type === 'log') {
      const logsContainer = document.getElementById('pageEditorLogsLogs')
      if (!logsContainer) {
        app.ports.pagesEditorLogsIn.send({
          type: 'LogReceived',
          log: event.data.log
        })
        return
      }

      const scrollTop = logsContainer.scrollTop
      const clientHeight = logsContainer.clientHeight
      const scrollHeight = logsContainer.scrollHeight
      if (scrollTop === scrollHeight - clientHeight) {
        setTimeout(() => logsContainer.scrollTop = Number.MAX_SAFE_INTEGER, 32)
      }
      app.ports.pagesEditorLogsIn.send({
        type: 'LogReceived',
        log: event.data.log
      })
    }
  })
}

export default {
  start
}
