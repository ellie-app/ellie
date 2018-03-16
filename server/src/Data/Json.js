exports._null = null

exports._mergeObjects = function (left) {
  return function (right) {
    return Object.assign({}, left, right)
  }
}
