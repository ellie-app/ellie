import * as SourceLoader from './SourceLoader'
import createChild from './Child.worker'

let nextId = 0
const genId = () => {
  const out = nextId
  nextId = (nextId + 1) % Number.MAX_SAFE_INTEGER
  return out
}

const startChild = url => {
  return new Promise((resolve, reject) => {
    const worker = createChild()
    let working = false

    const exec = (mtype) => (...args) => {
      working = true
      return new Promise((resolve, reject) => {
        const rid = genId()
        const onResolve = event => {
          const { id, type, success, result, message } = event.data
          if (type === mtype && id === rid) {
            worker.removeEventListener('message', onResolve)
            working = false
            if (success) resolve(result)
            else reject(Error(message))
          }
        }
        worker.addEventListener('message', onResolve)
        worker.postMessage({ type: mtype, id: rid, args })
      })
    }

    const onReady = event => {
      if (event.data.type === 'ready') {
        worker.removeEventListener('message', onReady)
        worker.postMessage({ type: 'load', args: [url] })
      }
    }

    const onLoad = event => {
      if (event.data.type === 'load') {
        resolve({
          compile: exec('compile'),
          parse: exec('parse'),
          idle: () => !working,
          worker
        })
        worker.removeEventListener('message', onLoad)
      }
    }

    worker.addEventListener('message', onReady)
    worker.addEventListener('message', onLoad)

    worker.postMessage({ type: 'ready' })
  })
}


let workQueue = []

export const init = (sourceUrl, onProgress) => {
  return SourceLoader
    .load(sourceUrl, onProgress)
    .then(url => {
      const numberOfWorkers = navigator.hardwareConcurrency || 4
      return Promise.all(
        [...Array(numberOfWorkers).keys()].map(() => startChild(url))
      )
    })
    .then(workers => {
      let requestId = 0

      const compile = (...args) => {
        return new Promise((resolve, reject) => {
          workQueue.push({ type: 'compile', args, resolve, reject })
          doWork()
        })
      }

      const parse = (...args) => {
        return new Promise((resolve, reject) => {
          workQueue.push({ type: 'parse', args, resolve, reject })
          doWork()
        })
      }

      const doWork = () => {
        workers.forEach(worker => {
          if (worker.idle()) {
            if (workQueue.length === 0) return
            const nextItem = workQueue.shift()
            worker[nextItem.type](...nextItem.args)
              .then(nextItem.resolve)
              .catch(nextItem.reject)
              .then(() => doWork())
          }
        })
      }

      return { compile, parse }
    })
}
