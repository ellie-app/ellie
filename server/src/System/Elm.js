var path = require('path')
var spawn = require('cross-spawn')
var binPath = path.resolve(__dirname, '../../../bin', 'elm')

function runProcess(command, args, options, callback) {
  try {
    var process = spawn(command, args, options)
    if (options.input) process.stdin.end(options.input)
    var stdout = '', stderr = ''
    process.stdout.on('data', function (data) { stdout += data })
    process.stderr.on('data', function (data) { stderr += data })
    process.on('close', function (code) {
      callback(null, { code: code, stderr: stderr, stdout: stdout })
    })
  } catch (e) {
    callback(e)
  }
}

exports._installByName = function _installByName(inputs) {
  return function _installByNameAff(fail, succeed) {
    runProcess(
      binPath,
      ['install', inputs.name],
      { cwd: inputs.root, env: process.env },
      function (error, result) {
        if (error) fail(error)
        else if (result.stderr) fail(Error(result.stderr))
        else succeed(inputs.helpers.unit)
      }
    )
  }
}

exports._reinstall = function _reinstall(inputs) {
  return function _reinstallAff(fail, succeed) {
    runProcess(
      binPath,
      ['install'],
      { cwd: inputs.root, env: process.env },
      function (error, result) {
        if (error) fail(error)
        else if (result.stderr) fail(Error(result.stderr))
        else succeed(inputs.helpers.unit)
      }
    )
  }
}

exports._compile = function _compile(inputs) {
  return function _compileAff(fail, succeed) {
    var debug = inputs.debug
    var entry = inputs.entry
    var output = inputs.output
    var root = inputs.root
    var helpers = inputs.helpers

    var args = debug ?
      ['make', entry, '--output', output, '--debug'] :
      ['make', entry, '--output', output]

    runProcess(
      binPath,
      args,
      { cwd: root, env: process.env },
      function (error, result) {
        if (error) fail(error)
        else if (result.stderr) succeed(helpers.left(result.stderr))
        else succeed(helpers.right(result.stdout))
      }
    )
  }
}

exports._format = function _format(inputs) {
  var code = inputs.code
  var helpers = inputs.helpers
  return function _formatAff(fail, succeed) {
    runProcess(
      'elm-format',
      ['--stdin'],
      { env: process.env, input: code },
      function (error, result) {
        if (error) fail(error)
        else if (result.code === 0) succeed(helpers.right(result.stdout))
        else succeed(helpers.left(result.stderr))
      }
    )
  }
}
