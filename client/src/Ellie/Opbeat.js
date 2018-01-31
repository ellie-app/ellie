import 'opbeat-js'

if (process.env.NODE_ENV === 'production') {
  window._opbeat('config', {
    orgId: OPBEAT_ORGANIZATION_ID,
    appId: OPBEAT_APP_ID
  })
}

const start = app => {
  app.ports.ellieOpbeatOut.subscribe(capture)
}

const capture = (exception) => {
  if (process.env.NODE_ENV === 'production') {
    const extraData = {
      tag: exception.tag,
      message: exception.message,
      line: exception.line,
      moduleName: exception.moduleName
    }

    exception.extraData.forEach(([key, value]) => {
      extraData[key] = value
    })

    window._opbeat('setExtraContext', extraData)
    window._opbeat('captureException', Error(extraData.message))
  }
}

export default {
  capture,
  start
}
