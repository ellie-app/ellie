exports._createClient = function _createClient(options) {
  return function _createClientEff() {
    var Aws = require('aws-sdk')
    return new Aws.S3(options)
  }
}

exports._putObject = function _putObject(inputs) {
  return function _putObjectAff(fail, succeed) {
    inputs.client.putObject({
      Body: new Buffer(inputs.options.contents, 'utf8'),
      Bucket: inputs.options.bucket,
      Key: inputs.options.key,
      ContentType: inputs.options.mimeType
    }, function _putObjectCallback(error, data) {
      if (error && typeof error.code === 'undefined') fail(error)
      else if (error) succeed(inputs.helpers.left(error))
      else succeed(inputs.helpers.right(inputs.helpers.unit))
    })
  }
}

exports._getObject = function _getObject(inputs) {
  return function _getObjectAff(fail, succeed) {
    inputs.client.getObject({
      Bucket: inputs.options.bucket, 
      Key: inputs.options.key
    }, function _getObjectCallback(error, data) {
      if (error && typeof error.code === 'undefined') fail(error)
      else if (error) succeed(inputs.helpers.left(error))
      else succeed(inputs.helpers.right(data.Body.toString()))
    })
  }
}

exports._headObject = function _headObject(inputs) {
  return function _headObjectAff(fail, succeed) {
    inputs.client.headObject({
      Bucket: inputs.options.bucket,
      Key: inputs.options.key
    }, function _headObjectCallback(error) {
      if (error && typeof error.code === 'undefined') fail(error)
      else if (error) succeed(inputs.helpers.left(error))
      else succeed(inputs.helpers.right(inputs.helpers.unit))
    })
  }
}

exports._listObjects = function _listObjects(inputs) {
  return function _listObjectsAff(fail, succeed) {
    var client = inputs.client
    var bucket = inputs.options.bucket
    var prefix = inputs.options.prefix
    var helpers = inputs.helpers
    client.listObjectsV2({
      Bucket: bucket,
      Prefix: prefix
    }, function (error, response) {
      if (error && typeof error.code === 'undefined') fail(error)
      else if (error) succeed(helpers.left(error))
      else succeed(helpers.right(response.Contents.map(function (a) { return a.Key })))
    })
  }
}