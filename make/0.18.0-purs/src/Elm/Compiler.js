exports._exec = null
exports._parseJson = null

;(function () {
  var nextId = 0
  function genId() {
    var out = nextId
    nextId = (nextId + 1) % Number.MAX_SAFE_INTEGER
    return out
  }

  function rpc(mtype, args, worker) {
    child.working = true
    return new Promise(function (resolve, reject) {
      var rid = genId()

      function onResolve(event) {
        var data = event.data
        if (data.type === mtype && data.id === rid) {
          child.worker.removeEventListener('message', onResolve)
          child.working = false
          if (data.success) resolve(data.result)
          else reject(Error(data.message))
        }
      }
      
      child.worker.addEventListener('message', onResolve)
      child.worker.postMessage({ type: mtype, id: rid, args: args })
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

  function repeatPromise(count, thunk) {
    var out = []
    for (var i = 0; i < count; i++) {
      out.push(thunk())
    }
    return Promise.all(out)
  }

  var workQueue = null
  var workers = null

  function ready() {
    return workQueue !== null && workers !== null
  }

  function makePool(compilerUrl) {
    return repeatPromise(
      navigator.hardwareConcurrency || 4,
      function () { return startChild(compilerUrl) }
    )
    .then(function (w) {
      workers = w
      workQueue = []
    })
  }

  function doWork() {
    if (!workers || !workQueue) return
    workers.forEach(function (worker) {
      if (worker.working) return
      if (workQueue.length === 0) return
      var nextItem = workQueue.shift()
      rpc(nextItem.type, nextItem.args, worker)
        .then(nextItem.resolve)
        .catch(nextItem.reject)
        .then(doWork)
    })
  }

  function enqueue(mtype, args, resolve, reject) {
    workQueue.push({
      type: mtype,
      args: args,
      resolve: resolve,
      reject: reject
    })
    doWork()
  }

  exports._exec = function _exec(ffiHelpers, compilerUrl, mtype, args) {
    return function runAff(fail, succeed) {
      if (!ready()) {
        makePool(compilerUrl)
          .then(function () {
            enqueue(
              mtype,
              args,
              function (result) { succeed(ffiHelpers.right(result)) },
              function (error) { succeed(ffiHelpers.left(error.message)) }
            )
          })
          .catch(fail)
      } else {
        enqueue(
          mtype,
          args,
          function (result) { succeed(ffiHelpers.right(result)) },
          function (error) { succeed(ffiHelpers.left(error.message)) }
        )
      }
    }
  }

  exports._parseJson = function _parseJson(right, left, input) {
    try {
      return right(JSON.parse(input))
    } catch (e) {
      return left(e.message)
    }
  }
}())