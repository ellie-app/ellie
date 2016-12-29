var CACHE_NAME =
  'ellie-cache-v1'

var isApiRequest = function (url) {
  return url.indexOf(API_BASE) !== -1
}

var cachedApiRequests =
  [
    /\/packages\/search/,
    /\/defaults\/revision/,
    /\/projects\/[a-fA-F0-9\-]+\/revisions\/[0-9]+/,
    /\/projects\/[a-fA-F0-9\-]+\/revisions\/latest/,
    /\/projects\/[a-fA-F0-9\-]+\/revisions\/[0-9]+/,
  ]

var isCachedApiRequest = function (url) {
  if (!isApiRequest(url)) {
    return false
  }

  var urlObj = new URL(url)

  return cachedApiRequests
    .some(function (regex) {
      return regex.test(urlObj.pathname)
    })
}


var fetchWithCache = function (request) {
  return caches
    .match(request)
    .then(function (response) {
      if (response) {
        return Promise.resolve(response)
      }

      return fetch(request.clone())
        .then(function (response) {
          if (!response || !response.ok || request.method !== 'GET') {
            return Promise.resolve(response)
          }

          return caches
            .open(CACHE_NAME)
            .then(function (cache) {
              cache.put(request, response.clone())
              return response
            })
        })
    })
}


if (process.env.NODE_ENV === 'production') {
  self.addEventListener('install', function (event) {
    event.waitUntil(
      caches
      .open(CACHE_NAME)
      .then(function (cache) {
        return cache.addAll([
          '/',
          '/new',
          '/1.1.js',
          '/app.js',
          '/main.css'
        ])
      })
    )
  })
}


self.addEventListener('fetch', function (event) {
  event.respondWith(
    isCachedApiRequest(event.request.url) ?
      fetchWithCache(event.request) :
      fetch(event.request)
  )
})
