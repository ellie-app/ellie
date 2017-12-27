exports._createClient = function _createClient(options) {
  return function _createClientAff(fail, succeed) {
    var Aws = require('aws-sdk')
    succeed(new Aws.S3(options))
  }
}

exports._putObject = function _putObject(unit, client, options) {
  return function _putObjectAff(fail, succeed) {
    client.putObject({
      Body: new Buffer(options.contents, 'utf8'),
      Bucket: options.bucket,
      Key: options.key,
      ContentType: options.mimeType
    }, function _putObjectCallback(error, data) {
      if (error) fail(error)
      else succeed(unit)
    })
  }
}

exports._getObject = function _getObject(client, options) {
  return function _getObjectAff(fail, succeed) {
    client.getObject({
      Bucket: options.bucket, 
      Key: options.key
    }, function _getObjectCallback(error, data) {
      if (error) fail(error)
      else succeed(data.Body.toString())
    })
  }
}

exports._headObject = function _headObject(unit, client, options) {
  return function _headObjectAff(fail, succeed) {
    client.headObject({
      Bucket: options.bucket,
      Key: options.key
    }, function _headObjectCallback(error) {
      if (error) fail(error)
      else succeed(unit)
    })
  }
}

exports._createPresignedPost = function _createPresignedPost(client, options) {
  return function _createPresignedPostAff(fail, succeed) {
    client.createPresignedPost({
      Bucket: options.bucket,
      Key: options.key,
      Field: options.fields.reduce(function (memo, pair) {
        memo[pair.key] = memo.value
        return memo
      }, {}),
      Conditions: options.fields.map(function (pair) {
        var out = {}
        out[pair.key] = pair.value
        return out
      })
    }, function _createPresignedPostCallback(error, data) {
      if (error) fail(error)
      else succeed(data)
    })
  }
}