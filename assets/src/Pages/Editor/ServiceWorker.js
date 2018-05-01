// import Toolbox from 'sw-toolbox'

// Toolbox.router.get(/\/api.*cache=permanent/, Toolbox.cacheFirst, {
//   cache: {
//     name: 'api-permanent-cache-v1'
//   }
// })

// Toolbox.router.get(/\api.*cache=temporary/, Toolbox.cacheFirst, {
//   cache: {
//     maxAgeSeconds: 60 * 15,
//     name: 'api-temporary-cache-v1'
//   }
// })

// Toolbox.router.get('/private/result/:hash', Toolbox.cacheFirst, {
//   cache: {
//     name: 'private-result-v1',
//     queryOptions: {
//       ignoreSearch: true
//     }
//   }
// })

self.addEventListener('fetch', (event) => {
  console.log(event.request.url)
})
