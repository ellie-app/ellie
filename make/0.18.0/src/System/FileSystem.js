exports._exists = null
exports._modified = null
exports._read = null
exports._write = null
exports._remove = null
exports._liftRawError = null

;(function () {
  var notFoundToken = {}
  var memory = {}
  var dbPromise = null
  var database = null

  function openDb() {
    if (!dbPromise) {
      dbPromise = new Promise(function (resolve, reject) {
        var open = indexedDB.open('Ellie:Store')
        open.onerror = reject

        open.onsuccess = function () {
          database = open.result
          resolve(open.result)
        }

        open.onupgradeneeded = function () {
          open.result.createObjectStore('Ellie:Store')
        }
      })
    }

    return dbPromise
  }

  function readFile(blob) {
    return new Promise(function (resolve, reject) {
      var fr = new FileReader()
      fr.onload = function () { resolve(fr.result) }
      fr.onerror = function () { reject(fr.error) }
      fr.readAsArrayBuffer(blob)
    })
  }

  function setIndexedDb(key, value) {
    if (!database) return openDb().then(function () { return setIndexedDb(key, value) })

    if (value.value instanceof Blob && navigator.platform === 'iPhone') {
      return readFile(value.value)
        .then(function (ab) {
          if (value.value instanceof File) {
            return setIndexedDb(key, { modified: value.modified, value: { data: ab, $: 'File', type: value.value.type, name: value.value.name } })
          }
          return setIndexedDb(key, { modified: value.modified, value: {  data: ab, $: 'Blob', type: value.value.type } })
        })
    }

    return new Promise(function (resolve, reject) {
      var store = database.transaction(['Ellie:Store'], 'readwrite').objectStore('Ellie:Store')
      var req = store.put(value, key)
      req.onerror = reject
      req.onsuccess = function(event) {
        resolve()
      }
    })
  }

  function getIndexedDb(key) {
    if (!database) return openDb().then(function () { return getIndexedDb(key) })

    return new Promise(function (resolve, reject) {
      var store = database.transaction(['Ellie:Store'], 'readwrite').objectStore('Ellie:Store')
      var req = store.get(key)
      req.onerror = reject
      req.onsuccess = function(event) {
        var value = event.target.result
        if (typeof event.target.result === 'undefined') {
          resolve(notFoundToken)
        } else if (navigator.platform === 'iPhone' && value.value.$ === 'File') {
          resolve({ modified: value.modified, value: new File(value.value.data, value.value.name, { type: value.value.type }) })
        } else if (navigator.platform === 'iPhone' && value.value.$ === 'Blob') {
          resolve({ modified: value.modified, value: new Blob(value.value.data, { type: value.value.type }) })
        } else {
          resolve(value)
        }
      } 
    })
  }

  function deleteStartsWithIndexedDb(key) {
    if (!database) return openDb().then(function () { return deleteStartsWithIndexedDb(key) })

    return new Promise(function (resolve, reject) {
      var store = database.transaction(['Ellie:Store'], 'readwrite').objectStore('Ellie:Store')
      var lower = key
      var upper = key.substring(0, key.length - 1) + String.fromCharCode(key.charCodeAt(key.length - 1) + 1)
      var range = IDBKeyRange.bound(lower, upper, false, true)
      var req = store.delete(range)
      req.onerror = reject
      req.onsuccess = function(event) {
        resolve()
      }
    })
  }

  function getMemory(key) {
    return memory.hasOwnProperty(key)
      ? memory[key]
      : notFoundToken
  }

  function setMemory(key, value) {
    memory[key] = value
  }

  function deleteStartsWithMemory(key) {
    for (var k in memory) {
      if (k.indexOf(key) === 0) {
        delete memory[k]
      }
    }
  }

  function get(key) {
    var value = getMemory(key)
    if (value !== notFoundToken) return Promise.resolve(value)
    return getIndexedDb(key)
      .then(function (value) {
        if (value === notFoundToken) return notFoundToken
        setMemory(key, value)
        return value
      })
  }

  function set(key, value) {
    return setIndexedDb(key, value)
      .then(function () {
        setMemory(key, value)
      })
  }

  function deleteStartsWith(key) {
    return deleteStartsWithIndexedDb(key)
      .then(function () {
        deleteStartsWithMemory(key)
      })
  }


  exports._exists = function (helpers, path) {
    return function aff(fail, succeed) {
      get(path)
        .then(function (value) { return succeed(value !== notFoundToken) })
    }
  }

  exports._modified = function (helpers, instant, path) {
    return function aff(fail, succeed) {
      get(path)
        .then(function (value) {
          if (value === notFoundToken) {
            succeed(helpers.nothing)
            return
          }

          var asInstant = instant(value.modified)

          helpers.isNothing(asInstant) ?
            fail({ $: 'CorruptModifiedTime', a: path }) :
            succeed(asInstant)
        })
    }
  }

  exports._read = function (helpers, path) {
    return function aff(fail, succeed) {
      get(path)
        .then(function (data) {
          data === notFoundToken ?
            fail({ $: 'FileNotFound', a: path }) :
            succeed(data.value)
        })
    }
  }

  exports._write = function (helpers, path, data) {
    return function aff(fail, succeed) {
      set(path, { value: data, modified: Date.now() })
        .then(function () { return succeed(helpers.unit) })
    }
  }

  exports._remove = function (helpers, path) {
    return function aff(fail, succeed) {
      deleteStartsWith(path)
        .then(function () { succeed(helpers.unit) })
    }
  }

  exports._liftRawError = function (FileNotFound, CorruptModifiedTime, raw) {
    if (raw.$ === 'FileNotFound') {
      return FileNotFound(raw.a)
    } else {
      return CorruptModifiedTime(raw.a)
    }
  }
}())