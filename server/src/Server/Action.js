exports._parseRequest = function (helpers) {
  return function (req) {
    return {
      url: helpers.parseUrl(req.originalUrl),
      method: helpers.parseMethod(req.method),
      headers: req.headers,
      params: req.params,
      vault: {}
    }
  }
}