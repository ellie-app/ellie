module.exports = function () {
  return new Promise(function (resolve, reject) {
    require.ensure([
      'codemirror/lib/codemirror',
      'codemirror/lib/codemirror.css',
      'codemirror/mode/elm/elm',
      'codemirror/mode/htmlmixed/htmlmixed',
      'codemirror/addon/lint/lint',
      'codemirror/addon/selection/active-line',
      'codemirror/addon/lint/lint.css',
      'codemirror/theme/material.css',
      './CodeMirror.css'
    ], function() {
      var CodeMirror = require('codemirror/lib/codemirror')
      require('codemirror/lib/codemirror.css')
      require('codemirror/mode/elm/elm')
      require('codemirror/mode/htmlmixed/htmlmixed')
      require('codemirror/addon/lint/lint')
      require('codemirror/addon/selection/active-line')
      require('codemirror/addon/lint/lint.css')
      require('codemirror/theme/material.css')
      require('./CodeMirror.css')
      window.CodeMirror = CodeMirror
      resolve()
    })
  })
}
