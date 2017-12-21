exports._succeed = null
exports._fail = null
exports._bind = null
exports._fromEffFnTask = null
exports._onError = null
exports._spawn = null
exports._kill = null
exports._liftEff = null
exports._wrapAVarOp = null

;(function () {
  function _succeed(value) {
    return {
      $: 'Succeed',
      value: value,
    }
  }
  exports._succeed = _succeed

  function _fail(error) {
    return {
      $: 'Fail',
      value: error,
    }
  }
  exports._fail = _fail

  function _binding(name, callback) {
    return {
      $: 'Binding',
      name: name,
      callback: callback,
      kill: null
    }
  }

  function _wrapAVarOp(ffiHelpers, name, run) {
    return _binding(name, function (callback) {
      return run(function avarCallback (either) {
        return function () {
          if (ffiHelpers.isLeft(either)) {
            callback(_fail(ffiHelpers.fromLeft(either)))
          } else {
            callback(_succeed(ffiHelpers.fromRight(either)))
          }
        }
      })()
    })
  }
  exports._wrapAVarOp = _wrapAVarOp

  function _fromEffFnTask(name, fn) {
    return _binding(name, function (callback) {
      return fn(
        function (error) { callback(_fail(error)) },
        function (value) { callback(_succeed(value)) }
      )
    })
  }
  exports._fromEffFnTask = _fromEffFnTask

  function _bind(callback, task) {
    return {
      $: 'Bind',
      callback: callback,
      task: task
    }
  }
  exports._bind = _bind

  function _onError(callback, task) {
    return {
      $: 'OnError',
      callback: callback,
      task: task
    }
  }
  exports._onError = _onError

  var _guid = 0


  function _spawn(task) {
    return function () {
      var proc = {
        $: 'Process',
        id: _guid++,
        root: task,
        stack: null,
      }
      _enqueue(proc)
      return proc
    }
  }
  exports._spawn = _spawn

  function _kill(unit, proc) {
    return function () {
      var task = proc.root
      if (task.$ === 'Binding' && task.kill) {
        task.kill()
      }
      proc.root = null
      return unit
    }
  }
  exports._kill = _kill

  function _liftEff(name, eff) {
    return _binding(name, function (callback) {
      try {
        callback(_succeed(eff()))
      } catch (error) {
        callback(_fail(error))
      }
    })
  }
  exports._liftEff = _liftEff

  var _working = false
  var _queue = []

  function _enqueue(proc) {
    _queue.push(proc)
    if (_working) return
    _working = true
    while (proc = _queue.shift()) {
      _step(proc)
    }
    _working = false
  }

  function _step(proc) {
    // console.log('Running process #' + proc.id)
    while (proc.root) {
      // console.log('Running task ' + debugInfo(proc.root))
      var rootTag = proc.root.$
      if (rootTag === 'Succeed' || rootTag === 'Fail') {
        while (proc.stack && proc.stack.$ !== rootTag) {
          proc.stack = proc.stack.rest
        }
        if (!proc.stack) {
          return
        }
        proc.root = proc.stack.callback(proc.root.value)
        proc.stack = proc.stack.rest
      } else if (rootTag === 'Binding') {
        proc.root.kill = proc.root.callback(function(newRoot) {
          proc.root = newRoot
          _enqueue(proc)
        })
        return
      } else if (rootTag === 'Bind' || rootTag === 'OnError') {
        proc.stack = {
          $: rootTag === 'Bind' ? 'Succeed' : 'Fail',
          callback: proc.root.callback,
          rest: proc.stack
        }
        proc.root = proc.root.task
      }
    }
    // console.groupEnd()
  }

  function debugInfo(task) {
    if (task.$ === 'Succeed') {
      return 'Succeed(' + JSON.stringify(task.value) + ')'
    } else if (task.$ === 'Fail') {
      return 'Fail(' + JSON.stringify(task.value) + ')'
    } else if (task.$ === 'Binding') {
      return 'Binding(' + task.name + ')'
    } else if (task.$ === 'Bind') {
      return 'Bind(' + debugInfo(task.task) + ')'
    } else if (task.$ === 'OnError') {
      return 'OnError(' + debugInfo(task.task) + ')'
    } else {
      return 'Unknown'
    }
  }
}())