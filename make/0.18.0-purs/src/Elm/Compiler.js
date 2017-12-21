exports._makeCompiler = null
exports._compile = null
exports._parseDependencies = null
exports._parseJson = null

;(function () {
  var nextId = 0
  function genId() {
    var out = nextId
    nextId = (nextId + 1) % Number.MAX_SAFE_INTEGER
    return out
  }

  function repeatPromise(count, thunk) {
    var out = []
    for (var i = 0; i < count; i++) {
      out.push(thunk())
    }
    return Promise.all(out)
  }

  function rpc(mtype, args, worker) {
    worker.working = true
    return new Promise(function (resolve, reject) {
      var rid = genId()

      function onResolve(event) {
        var data = event.data
        if (data.type === mtype && data.id === rid) {
          worker.worker.removeEventListener('message', onResolve)
          worker.working = false
          if (data.success) resolve(data.result)
          else reject(Error(data.message))
        }
      }
      
      worker.worker.addEventListener('message', onResolve)
      worker.worker.postMessage({ type: mtype, id: rid, args: args })
    }) 
  }

  function startChild(url) {
    return new Promise(function (resolve, reject) {
      var worker = new Worker(url)
      worker.addEventListener('message', onReady)
      worker.addEventListener('message', onLoad)
      worker.postMessage({ type: 'ready' })
      
      function onReady(event) {
        if (event.data.type === 'ready') {
          worker.removeEventListener('message', onReady)
          worker.postMessage({ type: 'load', args: [url] })
        }
      }

      function onLoad(event) {
        if (event.data.type === 'load') {
          resolve({        
            worker: worker,
            working: false
          })
          worker.removeEventListener('message', onLoad)
        }
      }
    })
  }

  function makePool(compilerUrl) {
    return repeatPromise(
      navigator.hardwareConcurrency || 4,
      function () { return startChild(compilerUrl) }
    )
  }

  function Compiler(workers) {
    this._workers = workers
    this._queue = []
  }

  Compiler.prototype._doWork = function _doWork() {
    var self = this
    self._workers.forEach(function (worker) {
      if (worker.working) return
      if (self._queue.length === 0) return
      var nextItem = self._queue.shift()
      rpc(nextItem.type, nextItem.args, worker)
        .then(nextItem.resolve)
        .catch(nextItem.reject)
        .then(function () { self._doWork() })
    })
  }

  Compiler.prototype._enqueue =  function _enqueue(mtype, args, resolve, reject) {
    this._queue.push({
      type: mtype,
      args: args,
      resolve: resolve,
      reject: reject
    })
    this._doWork()
  }

  Compiler.prototype.parseDependencies = function parseDependencies(args) {
    var self = this
    return new Promise(function (resolve, reject) {
      self._enqueue('parse', args, resolve, reject)
    })
  }

  Compiler.prototype.compile = function compile(args) {
    var self = this
    return new Promise(function (resolve, reject) {
      self._enqueue('compile', args, resolve, reject)
    })
  }
  

  exports._makeCompiler = function _makeCompiler(compilerUrl) {
    return function (fail, succeed) {
      makePool(compilerUrl)
        .then(function (workers) {
          return new Compiler(workers)
        })
        .then(succeed)
        .catch(function (error) { fail(error.message) })
    }
  }

  exports._compile = function _compile(compiler, args) {
    return function (fail, succeed) {
      compiler
        .compile(args)
        .then(succeed)
        .catch(function (error) { fail(error.message) })
    }
  }

  exports._parseDependencies = function _parseDependencies(compiler, args) {
    return function (fail, succeed) {
      compiler
        .parseDependencies(args)
        .then(succeed)
        .catch(function (error) { fail(error.message) })
    }
  }

  exports._parseJson = function _parseJson(ffiHelpers, input) {
    try {
      return ffiHelpers.right(JSON.parse(input))
    } catch (e) {
      return ffiHelpers.left(e.message)
    }
  }
}())