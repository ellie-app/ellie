var _user$project$Native_Storage = (function () {
  var IndexedDbStore = (function () {
    var database = null
    var dbPromise = null

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

    function set(db, key, value) {
      return new Promise(function (resolve, reject) {
        var store = db.transaction(['Ellie:Store'], 'readwrite').objectStore('Ellie:Store')
        var req = store.put(value, key)
        req.onerror = reject
        req.onsuccess = function(event) {
          resolve()
        }
      })
    }

    function get(db, key) {
      return new Promise(function (resolve, reject) {
        var store = db.transaction(['Ellie:Store'], 'readwrite').objectStore('Ellie:Store')
        var req = store.get(key)
        req.onerror = reject
        req.onsuccess = function(event) {
          resolve(event.target.result)
        }
      })
    }

    return {
      get: function(key) {
        if (!database) {
          return openDb().then(function () { return this.get(key) }.bind(this))
        }

        return get(database, key)
      },
      set: function(key, value) {
        if (!database) {
          return openDb().then(function () { return this.set(key, value) }.bind(this))
        }

        return set(database, key, value)
      }
    }
  }())

  var MemoryStore = (function () {
    var memory = {}

    return {
      get: function(key) {
        if (memory.hasOwnProperty(key)) return Promise.resolve(memory[key])
        return Promise.resolve(null)
      },
      set: function(key, value) {
        memory[key] = value
        return Promise.resolve()
      }
    }
  }())


  var get = function (key) {
    return _elm_lang$core$Native_Scheduler.nativeBinding(function (callback) {
      MemoryStore.get(key)
        .then(function (value) {
          if (value != null) return value
          return IndexedDbStore.get(key)
        })
        .then(function (value) {
          if (value == null) return null
          return MemoryStore.set(key, value)
            .then(function () { return value })
        })
        .then(function (value) {
          callback(_elm_lang$core$Native_Scheduler.succeed(value))
        })
        .catch(function (error) {
          callback(_elm_lang$core$Native_Scheduler.fail(error.message))
        })
    })
  }


  var set = F2(function(key, value) {
    return _elm_lang$core$Native_Scheduler.nativeBinding(function (callback) {
      Promise.all([
        IndexedDbStore.set(key, value),
        MemoryStore.set(key, value)
      ])
      .then(function () {
        callback(_elm_lang$core$Native_Scheduler.succeed(value));
      })
      .catch(function (error) {
        callback(_elm_lang$core$Native_Scheduler.fail(error.message))
      })
    })
  })

  return {
    get: get,
    set: set
  }
}())
