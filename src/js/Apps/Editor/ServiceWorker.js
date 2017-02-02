var GENERAL_CACHE_NAME =
  'ellie-cache-v4'

var SEARCH_CACHE_NAME =
  'ellie-cache-search-v1'

var isApiRequest = function (url) {
  return url.indexOf(API_BASE) !== -1
}

var searchRegex = /\/packages\/search/;

var isCachedSearchRequest = function (url) {
  var urlObj = new URL(url);
  return searchRegex.test(urlObj.pathname);
}

var cachedApiRequests =
  [
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

var fetchWithCache = function (cacheName, request) {
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
            .open(cacheName)
            .then(function (cache) {
              cache.put(request, response.clone())
              return response
            })
        })
    })
}

var clearSearchInterval =
  process.env.NODE_ENV === 'production' ?
    10 * 60 * 1000 :
    10 * 1000


setInterval(function () {
  caches.delete(SEARCH_CACHE_NAME)
}, clearSearchInterval)

self.addEventListener('fetch', function (event) {
  if (isCachedSearchRequest(event.request.url)) {
    event.respondWith(
      fetchWithCache(SEARCH_CACHE_NAME, event.request))
  } else if (isCachedRequest(event.request.url)) {
    event.respondWith(
      fetchWithCache(GENERAL_CACHE_NAME, event.request))
  } else {
    event.respondWith(
      fetch(event.request))
  }
})
