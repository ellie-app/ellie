function parseHeaders(rawHeaders) {
	var headers = {}
	var headerPairs = rawHeaders.split('\u000d\u000a');
	for (var i = headerPairs.length; i--; ) {
		var headerPair = headerPairs[i];
		var index = headerPair.indexOf('\u003a\u0020');
		if (index > 0) {
			var key = headerPair.substring(0, index);
			var value = headerPair.substring(index + 2);
      if (headers.hasOwnProperty(key)) {
        headers[key] = value + ', ' + headers[key]
      } else {
        headers[key] = value
      }
		}
	}
	return headers
}

exports._send = function _send(ffiHelpers, errors, showUrl, request) {
	return function runAff(fail, succeed) {
		var xhr = new XMLHttpRequest()

		xhr.addEventListener('error', function() {
			succeed(ffiHelpers.left(errors.networkError))
		})

		xhr.addEventListener('timeout', function() {
			succeed(ffiHelpers.left(errors.timeout))
		})

		xhr.addEventListener('load', function() {
      var response = {
        url: xhr.responseURL,
        code: xhr.status,
        message: xhr.statusText,
        headers: parseHeaders(xhr.getAllResponseHeaders()),
        body: xhr.response
      }

      if (xhr.status < 200 || 300 <= xhr.status) {
        response.body = xhr.responseText
        return succeed(ffiHelpers.left(errors.badStatus(response)))
      }

      var result = request.expect.responseToResult(response)

      if (ffiHelpers.isLeft(result)) {
        response.body = xhr.responseText
        return succeed(ffiHelpers.left(errors.badPayload(ffiHelpers.fromLeft(result), response)))
      } else {
        return succeed(result)
      }
		})

		try {
			xhr.open(request.method, showUrl(request.url), true)
		} catch (e) {
			return succeed(ffiHelpers.left(errors.barUrl(request.url)))
		}

    var headers = request.headers
  	for (var i = 0; i < headers.length; i++) {
  		xhr.setRequestHeader(headers[i].key, headers[i].value)
  	}

  	xhr.responseType = request.expect.responseType
  	xhr.withCredentials = request.withCredentials

  	if (ffiHelpers.isJust(request.timeout)) {
  		xhr.timeout = ffiHelpers.fromJust(request.timeout)
    }
    
    if (ffiHelpers.isJust(request.progress)) {
      var handler = ffiHelpers.fromJust(request.progress)
      xhr.addEventListener('progress', function (ev) {
        handler({ total: ev.total, loaded: ev.loaded })
      })
    }

		xhr.send()

		return function() {
      return function cancelAff(fail, succeed) {
        try {
          xhr.abort()
          succeed()
        } catch (e) {
          fail(e)
        }
      }
    }
	}
}

function expect(type) {
  return function (responseToResult) {
    return {
      $: 'Expect',
      responseType: type,
      responseToResult: responseToResult
    }
  }
}

exports._parseJson = function _parseJson(right, left, string) {
  try {
    return right(JSON.parse(string))
  } catch (e) {
    return left(e.message)
  }
}

exports._expectStringResponse = expect('text')
exports._expectBlobResponse = expect('blob')
exports._expectUntypedResponse = expect('')
