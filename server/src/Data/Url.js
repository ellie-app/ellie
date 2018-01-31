var url = require('url')

exports._parse = function _parse(Just) {
  return function (Nothing) {
    return function (string) {
      var parsed = url.parse(string)
      var params = new url.URLSearchParams(parsed.query)
      var paramsMap = {}
      var iter = params.entries()
      var next = iter.next()
      while(!next.done) {
        if (!paramsMap.hasOwnProperty(next.value[0])) paramsMap[next.value[0]] = []
        paramsMap[next.value[0]].push(next.value[1])
        next = iter.next()
      }
      return {
        protocol: parsed.protocol ? Just(parsed.protocol) : Nothing,
        username: parsed.username ? Just(parsed.username) : Nothing,
        password: parsed.password ? Just (parsed.password) : Nothing,
        hostname: parsed.hostname ? Just(parsed.hostname) : Nothing,
        port: parsed.port ? Just(parseInt(parsed.port, 10)) : Nothing,
        pathname: parsed.pathname,
        query: paramsMap,
        hash: parsed.hash ? Just(parsed.hash) : Nothing
      }
    }
  }
}