exports._get = function _get(url) {
  return function(fail, succeed) {
    var request = require('request')
    request.get({url: url, json: true}, function (error, response, body) {
      if (error) fail(error)
      else succeed(body)
    })
  }  
}

exports._download = function _download(unit) {
  return function(url) {
    return function(target) {
      return function(fail, succeed) {
        var fs = require('fs')
        var request = require('request')
        request(url)
          .on('error', failed)
          .pipe(fs.createWriteStream(target))
          .on('end', function () { succeed(unit) })
      }
    }
  }
}