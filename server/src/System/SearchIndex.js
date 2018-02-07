exports._create = function _createAff(fail, succeed) {
  var elasticlunr = require('elasticlunr')
  var index = elasticlunr(function () {
    this.addField('user')
    this.addField('project')
    this.addField('description')
    this.setRef('id')
  })
  succeed(index)
}

exports._add = function _add(unit, index, documents) {
  return function _addAff(fail, succeed) {
    documents.forEach(function (document) {
      index.addDoc(document)
    })
    succeed(unit)
  }
}

exports._search = function _search(index, query, limit) {
  return function _searchAff(fail, succeed) {
    var parsedQuery
    var slashIndex = query.indexOf('/')
    if (slashIndex !== -1 && slashIndex !== query.length - 1) {
      parsedQuery = {
        user: query.split('/')[0],
        project: query.split('/')[1]
      }
    } else {
      parsedQuery = query.replace('/', '')
    }

    var result = index.search(parsedQuery, {
      fields: {
        user: { boost: 3 },
        project: { boost: 2 },
        description: { boost: 1 }
      },
      expand: true
    }).map(function (r) {
      return index.documentStore.getDoc(r.ref)
    }).slice(0, limit)

    succeed(result)
  }
}