exports._create = function _createAff(fail, succeed) {
  var SearchIndex = require('search-index')
  SearchIndex({}, function(error, index) {
    if (error) fail(error)
    else succeed(index)
  })
}

exports._add = function _add(unit, index, documents) {
  return function _addAff(fail, succeed) {
    index.concurrentAdd({}, documents, function(error) {
      if (error) fail(error)
      else succeed(unit)
    })
  }
}

exports._flush = function _flush(unit, index) {
  return function _flushAff(fail, succeed) {
    index.flush(function (error) {
      if (error) fail(error)
      else succeed(unit)
    })
  }
}

exports._search = function _search(index, query, limit) {
  return function _searchAff(fail, succeed) {
    var data = []
    var failed = false
    index
      .search({ query: query, pageSize: limit })
      .on('end', function () {
        if (failed) return
        succeed(data.reverse())
      })
      .on('error', function (error) {
        failed = true
        fail(error)
      })
      .on('data', function (document) {
        data.push(document)
      })
  }
}