var CACHE_NAME =
  'ellie-embed-cache-v2'

var isApiRequest = function (url) {
  return url.indexOf(API_BASE) !== -1
}

var cachedApiRequests =
  [
    /\/packages\/search/,
    /\/defaults\/revision/,
    /\/projects\/[a-zA-Z0-9-_]+\/revisions\/[0-9]+/,
  ]

var cachedResourceRequests =
  [
    /\/app\..+\.js/,
    /\/chunk\..+\.js/,
    /\/main\..+\.css/,
    /\/images\/\.+/
  ]

var isCachedResourceRequest = function (url) {
  var urlObj = new URL(url)
  return cachedResourceRequests
    .some(function (regex) {
      return regex.test(urlObj.pathname)
    })
}

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

var isCachedRequest = function (url) {
  return isCachedApiRequest(url) ||
    isCachedResourceRequest(url)
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


self.addEventListener('fetch', function (event) {
  event.respondWith(
    isCachedRequest(event.request.url) ?
      fetchWithCache(event.request) :
      fetch(event.request)
  )
})
