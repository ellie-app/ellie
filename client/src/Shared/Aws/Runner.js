import captureException from '../Opbeat'

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
        captureException({
          tag: 'UnknownOutboundPortMessage',
          message: `Unexpected port message from Shared.Aws: "${data.tag}"`,
          line: 11,
          moduleName: 'Shared/Aws/Runner.js',
          extraData: data
        })
        return
    }
  })

  const uploadBatch = (id, infos) => {
    return Promise
      .all(
        infos.map(info => {
          return fileFromContent(info.content, info.name, info.mime)
            .then(uploadToAws(info.url, info.fields))
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
    fileFromContent(info.content, info.name, info.mime)
      .then(uploadToAws(info.url, info.fields))
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

  const uploadToAws = (url, fields) => file => {
    return new Promise((resolve, reject) => {
      const formData = new FormData();
      fields.forEach(field => formData.append(field[0], field[1]))
      formData.append('file', file)

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

  const fileFromContent = (content, filename, mimeType) => {
    switch(content.tag) {
      case 'Stream':
        return readStreamedContent(content.url, filename, mimeType)
      case 'Direct':
        return createDirectFile(content.data, filename, mimeType)
    }
  }

  const createDirectFile = (data, filename, mimeType) => {
    const file = new File([data], filename, { type: mimeType })
    return Promise.resolve(file)
  }

  const readStreamedContent = (url, filename, mimeType) => {
    return new Promise((resolve, reject) => {
      let failed = false
      const xhr = new XMLHttpRequest()

      xhr.responseType = 'arraybuffer'

      xhr.addEventListener('error', () => {
        if (failed) return
        failed = true
        debugger
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

        const file = new File([body], filename, { type: mimeType })
        resolve(file)
  		})

      xhr.open('GET', url, true)
      xhr.send()
    })
  }
}


export default {
  start
}
