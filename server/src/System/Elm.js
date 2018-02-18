var spawn = require('cross-spawn');

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

exports._init = function _init(inputs) {
  return function _initAff(fail, succeed) {
    var root = inputs.root
    var helpers = inputs.helpers

    runProcess(
      'elm-package',
      ['install', '--yes'],
      { cwd: root, env: process.env },
      function (error, result) {
        if (error) fail(error)
        else if (result.code === 0) succeed(helpers.right(result.stdout))
        else succeed(helpers.left(result.stderr))
      }
    )
  }
}

exports._install = function _install(inputs) {
  return function _installAff(fail, succeed) {
    var root = inputs.root
    var name = inputs.name
    var version = inputs.version
    var helpers = inputs.helpers

    runProcess(
      'elm-package',
      ['install', name, version, '--yes'],
      { cwd: root, env: process.env },
      function (error, result) {
        if (error) fail(error)
        else if (result.code === 0) succeed(helpers.right(result.stdout))
        else succeed(helpers.left(result.stderr))
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
      [ entry, '--output', output, '--debug', '--yes', '--report', 'json' ] :
      [ entry, '--output', output, '--yes', '--report', 'json' ]

    runProcess(
      'elm-make',
      args,
      { cwd: root, env: process.env },
      function (error, result) {
        if (error) fail(error)
        else if (result.code === 0) succeed(helpers.right(result.stdout))
        else if (result.stdout) succeed(helpers.right(result.stdout))
        else succeed(helpers.left(result.stderr))
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