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

  function setIndexedDb(key, value) {
    if (!database) return openDb().then(function () { return setIndexedDb(key, value) })

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
        if (typeof event.target.result === 'undefined') resolve(notFoundToken)
        else resolve(event.target.result)
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
        .then(function (value) { return helpers.right(value !== notFoundToken) })
        .then(succeed)
        .catch(fail)
    }
  }

  exports._modified = function (helpers, instant, path) {
    return function aff(fail, succeed) {
      get(path)
        .then(function (value) {
          if (value === notFoundToken) {
            return helpers.right(helpers.nothing)
          }

          var asInstant = instant(value.modified)

          return helpers.isNothing(asInstant) ?
            helpers.left({ $: 'CorruptModifiedTime', a: path }) :
            helpers.right(asInstant)
        })
        .then(succeed)
        .catch(fail)
    }
  }

  exports._read = function (helpers, path) {
    return function aff(fail, succeed) {
      get(path)
        .then(function (data) {
          return data === notFoundToken ?
            helpers.left({ $: 'FileNotFound', a: path }) :
            helpers.right(data.value)
        })
        .then(succeed)
        .catch(fail)
    }
  }

  exports._write = function (helpers, path, data) {
    return function aff(fail, succeed) {
      set(path, { value: data, modified: Date.now() })
        .then(helpers.const(helpers.right(helpers.unit)))
        .then(succeed)
        .catch(fail)
    }
  }

  exports._remove = function (helpers, path) {
    return function aff(fail, succeed) {
      deleteStartsWith(path)
        .then(helpers.const(helpers.right(helpers.unit)))
        .then(succeed)
        .catch(fail)
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