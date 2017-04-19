function openDb() {
  return new Promise(function (resolve, reject) {
    var open = indexedDB.open('ElmCompiler:SourceLoader')
    open.onerror = reject

    open.onsuccess = function () {
      resolve(open.result)
    }

    open.onupgradeneeded = function () {
      open.result.createObjectStore('ElmCompiler:SourceLoader')
    }
  })
}

function saveBlob(db, blob) {
  return new Promise(function (resolve, reject) {
    var store = db.transaction(['ElmCompiler:SourceLoader'], 'readwrite').objectStore('ElmCompiler:SourceLoader')
    var req = store.put(blob, 'script')
    req.onerror = reject
    req.onsuccess = function(event) {
      resolve(blob)
    }
  })
}

function loadBlob(db) {
  return new Promise(function (resolve, reject) {
    var store = db.transaction(['ElmCompiler:SourceLoader'], 'readwrite').objectStore('ElmCompiler:SourceLoader')
    var req = store.get('script')
    req.onerror = reject
    req.onsuccess = function(event) {
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

function fetchScript(path, onProgress) {
  return new Promise(function (resolve, reject) {
    var request = new XMLHttpRequest()
    request.addEventListener('error', function() {
      reject({status: request.status, text: "could not load " + path})
    })
    request.addEventListener('load', function() {
      if (request.status != 200) {
        reject({status: request.status, text: request.responseText})
      } else {
        resolve(new Blob([request.responseText]))
      }
    })
    request.addEventListener('progress', onProgress)
    request.open('GET', path)
    request.setRequestHeader('Accept', 'application/javascript')
    request.send()
  })
}

function executeScript(blob) {
  return new Promise(function (resolve, reject) {
    var url = URL.createObjectURL(blob)
    importScripts(url)
    resolve(require('Dll/ElmCompiler'))
  })
}

function load(url, onProgress) {
  return openDb()
    .then(function (db) {
      return loadBlob(db)
        .then(executeScript)
        .catch(function (error) {
          return fetchScript(url, onProgress)
            .then(function (blob) { return saveBlob(db, blob) })
            .then(executeScript)
        })
        .then(function (result) { db.close(); return result; })
        .catch(function (error) { db.close(); return Promise.reject(error); })
    })
}

module.exports = {
  load: load
}
