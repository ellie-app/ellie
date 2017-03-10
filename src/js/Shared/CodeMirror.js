module.exports = function (vimMode) {
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

      require('./CodeMirror.css')
      require('codemirror/addon/lint/lint.css')
      require('codemirror/lib/codemirror.css')
      require('codemirror/theme/material.css')
      var CodeMirror = require('codemirror/lib/codemirror')
      require('codemirror/mode/elm/elm')
      require('codemirror/mode/htmlmixed/htmlmixed')
      require('codemirror/addon/lint/lint')
      require('codemirror/addon/selection/active-line')
      window.CodeMirror = CodeMirror

      if (!vimMode) {
        resolve()
        return
      }

      require.ensure([
        'codemirror/keymap/vim',
        'codemirror/addon/dialog/dialog',
        'codemirror/addon/dialog/dialog.css'
      ], function () {

        require('codemirror/keymap/vim')
        require('codemirror/addon/dialog/dialog')
        require('codemirror/addon/dialog/dialog.css')
        resolve()
      })
    })
  })
}
