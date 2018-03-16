exports._exec = function _execOne(client, name, inputs) {
  return function _execAff(fail, succeed) {
    var invocation = 'SELECT ' + name + '($1) AS result;'

    var statement = {
      name: name,
      text: invocation,
      values: [JSON.stringify(inputs)]
    }

    client.query(statement, function (error, result) {
      if (error) {
        fail(error)
      } else {
        console.log(result)
        if (typeof result.rows[0].result === 'undefined') succeed(null)
        else succeed(result.rows[0].result)
      }
    })
  }
}

exports._connect = function _connect(connection) {
  return function _connectAff(fail, succeed) {
    var pg = require('pg')
    var client = new pg.Client(connection)
    client
      .connect()
      .then(function () { succeed(client) })
      .catch(fail)
  }
}
