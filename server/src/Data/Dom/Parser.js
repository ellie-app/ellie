var DOMParser = require('xmldom')

exports._parse = function _parse(input) {
  var parser = new DOMParser()
  return parser.parseFromString(input)
}
