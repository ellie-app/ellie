const hackCompilerSizes = {
  '0.18.0': 4730627
}

const withDb = (fn) => {
  return new Promise((resolve, reject) => {
    var open = indexedDB.open('ElmCompiler:SourceLoader')
    open.onerror = reject

    open.onsuccess = function () {
      resolve(open.result)
    }

    open.onupgradeneeded = function () {
      open.result.createObjectStore('ElmCompiler:SourceLoader')
    }
  })
  .then(db => {
    return fn(db)
      .then(result => { db.close(); return result; })
      .catch(error => { db.close(); return Promise.reject(error); })
  })
}

const saveBlob = (db, blob) => {
  return new Promise(function (resolve, reject) {
    var store = db.transaction(['ElmCompiler:SourceLoader'], 'readwrite').objectStore('ElmCompiler:SourceLoader')
    var req = store.put(blob, 'script')
    req.onerror = reject
    req.onsuccess = function(event) {
      resolve(blob)
    }
  })
}

const loadBlob = db => {
  return new Promise(function (resolve, reject) {
    var store = db.transaction(['ElmCompiler:SourceLoader'], 'readwrite').objectStore('ElmCompiler:SourceLoader')
    var req = store.get('script')
    req.onerror = reject
    req.onsuccess = event => {
      if (!event.target.result) reject(Error('script not loaded yet'))
      else {
        if (event.target.result instanceof Blob) {
          resolve(event.target.result)
        } else {
          resolve(dataUriToBlob(event.target.result))
        }
      }
    }
  })
}

const fetchScript = (path, onProgress) => {
  return new Promise((resolve, reject) => {
    var request = new XMLHttpRequest()
    request.addEventListener('error', () => {
      reject({status: request.status, text: "could not load " + path})
    })
    request.addEventListener('load', () => {
      if (request.status != 200) {
        reject({status: request.status, text: request.responseText})
      } else {
        onProgress(1)
        resolve(new Blob([request.responseText]))
      }
    })
    request.addEventListener('progress', event => onProgress(event.loaded/hackCompilerSizes['0.18.0']))
    request.open('GET', path)
    request.setRequestHeader('Accept', 'application/javascript')
    request.send()
  })
}

const load = (url, onProgress) =>
  withDb(db => {
    return loadBlob(db)
      .then(blob => URL.createObjectURL(blob))
      .catch(error => {
        return fetchScript(url, onProgress)
          .then(blob => saveBlob(db, blob))
          .then(blob => URL.createObjectURL(blob))
      })
  })

module.exports = {
  load
}
