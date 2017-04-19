function openDb(storeName) {
  return new Promise(function (resolve, reject) {
    var open = indexedDB.open(storeName, 1)
    open.onsuccess = function () { resolve(open.result) }
    open.onerror = reject
    open.onupgradeneeded = function () {
      open.result.createObjectStore(storeName, { keyPath: 'itemKey' })
    }
  })
}

function IdbDirectStorage(storeName) {
  this._storeName = storeName
  this._cache = null
  this._opsQueue = []
  this._runningQueue = false
  this._initializingPromise = null
  this._initialized = false
}

IdbDirectStorage.prototype = {
  _assertInitialized: function (method) {
    if (!this._initialized) {
      throw Error(`Cannot call method ${method} before initializing. Wait for initialize() to finish!`)
    }
  },

  _runQueue: function () {
    if (!this._opsQueue.length) return
    if (this._timeout) clearTimeout(this._timeout)
    var self = this
    this._timeout = setTimeout(function () {
      openDb(self._storeName)
        .then(function (db) {
          return new Promise(function (resolve, reject) {
            var tx = db.transaction(self._storeName, 'readwrite')
            var store = tx.objectStore(self._storeName)
            tx.oncomplete = resolve
            tx.onerror = reject

            self._opsQueue.forEach(function (op) {
              if (op.type === 'delete') store.delete(op.itemKey)
              else if (op.type === 'put') store.put({ itemKey: op.itemKey, itemValue: op.itemValue })
              else if (op.type === 'clear') store.clear()
            })
          })
          .then(function () { db.close() })
          .catch(function () { db.close() })
        })
    }, 100)
  },

  initialize: function () {
    if (this._initialized) return Promise.resolve()

    var self = this
    if (this._initializingPromise) {
      return this._initializingPromise
    }

    this._initializingPromise =
      openDb(this._storeName)
        .then(function (db) {
          return new Promise(function (resolve, reject) {
            var request = db
              .transaction(self._storeName, 'readwrite')
              .objectStore(self._storeName)
              .getAll()

            request.onerror = reject
            request.onsuccess = resolve
          })
          .then(function (event) {
            self._cache =
              event.target.result.reduce(function (memo, next) {
                memo[next.itemKey] = next.itemValue
                return memo
              }, {})

            return db.close()
          })
          .then(function () {
            self._initialized = true
            self._initializingPromise = null
          })
        })

    return this._initializingPromise
  },

  getItem: function (itemKey) {
    this._assertInitialized('getItem()')
    return this._cache.hasOwnProperty(itemKey) ? this._cache[itemKey] : undefined
  },

  setItem: function (itemKey, itemValue) {
    this._opsQueue.push({ type: 'put', itemKey, itemValue: String(itemValue) })
    this._runQueue()
    return this._cache[itemKey] = String(itemValue)
  },

  removeItem: function (itemKey) {
    this._opsQueue = this._opsQueue.filter(function (o) { return !(o.type === itemKey && o.type === 'put') })
    this._opsQueue.push({ type: 'delete', itemKey })
    this._runQueue()
    return delete this._cache[itemKey]
  },

  clear: function () {
    this._opsQueue = [{ type: 'clear' }]
    this._runQueue()
    return this._cache = {}
  }
}

module.exports = IdbDirectStorage
