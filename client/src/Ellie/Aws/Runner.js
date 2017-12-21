import OpbeatRunner from '../Opbeat/Runner'

const start = app => {
  app.ports.sharedAwsOut.subscribe(data => {
    switch (data.tag) {
      case 'UploadStart':
        uploadStart(data.id, data.info)
        return

      case 'UploadBatch':
        uploadBatch(data.id, data.infos)
        return

      default:
        OpbeatRunner.capture({
          tag: 'UnknownOutboundPortMessage',
          message: `Unexpected port message from Shared.Aws: "${data.tag}"`,
          line: 11,
          moduleName: 'Ellie/Aws/Runner.js',
          extraData: data
        })
        return
    }
  })

  const uploadBatch = (id, infos) => {
    return Promise
      .all(
        infos.map(info => {
          return blobFromContent(info.content, info.mime)
            .then(uploadToAws(info.url, info.name, info.fields))
        })
      )
      .then(() => {
        app.ports.sharedAwsIn.send({
          tag: 'UploadSucceeded',
          id: id,
        })
      })
      .catch(error => {
        app.ports.sharedAwsIn.send({
          tag: 'UploadFailed',
          id: id,
          message: error.message
        })
      })
  }

  const uploadStart = (id, info) => {
    blobFromContent(info.content, info.mime)
      .then(uploadToAws(info.url, info.name, info.fields))
      .then(() => {
        app.ports.sharedAwsIn.send({
          tag: 'UploadSucceeded',
          id: id,
        })
      })
      .catch(error => {
        app.ports.sharedAwsIn.send({
          tag: 'UploadFailed',
          id: id,
          message: error.message
        })
      })
  }

  const uploadToAws = (url, filename, fields) => blob => {
    return new Promise((resolve, reject) => {
      const formData = new FormData();
      fields.forEach(field => formData.append(field[0], field[1]))
      formData.append('file', blob, filename)

      let failed = false
      const xhr = new XMLHttpRequest()

      xhr.addEventListener('error', () => {
        if (failed) return
        failed = true
        reject(Error(`Failed to read content from ${url}`))
      })

      xhr.addEventListener('load', function() {
        if (failed) return

        const code = xhr.status
        const statusText = xhr.statusText

        if (code < 200 || 300 < code) {
          failed = true
          reject(Error(`Failed to read content from ${url}`))
          return
        }

        resolve()
      })

      xhr.open('POST', url)
      xhr.send(formData)
    })
  }

  const blobFromContent = (content, mimeType) => {
    switch(content.tag) {
      case 'Stream':
        return readStreamedContent(content.url, mimeType)
      case 'Direct':
        return createDirectBlob(content.data, mimeType)
    }
  }

  const createDirectBlob = (data, mimeType) => {
    const blob = new Blob([data], { type: mimeType })
    return Promise.resolve(blob)
  }

  const readStreamedContent = (url, mimeType) => {
    return new Promise((resolve, reject) => {
      let failed = false
      const xhr = new XMLHttpRequest()

      xhr.responseType = 'arraybuffer'

      xhr.addEventListener('error', () => {
        if (failed) return
        failed = true
        reject(Error(`Failed to read content from ${url}`))
      })

      xhr.addEventListener('load', function() {
        if (failed) return

        const body = xhr.response
        const code = xhr.status
        const statusText = xhr.statusText

        if (code < 200 || 300 < code) {
          failed = true
          reject(Error(`Failed to read content from ${url}`))
          return
        }

        const blob = new Blob([body], { type: mimeType })
        resolve(blob)
  		})

      xhr.open('GET', url, true)
      xhr.send()
    })
  }
}


export default {
  start
}
