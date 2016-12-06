const CodeMirror = require('codemirror/lib/codemirror')
require('codemirror/lib/codemirror.css')
require('codemirror/mode/elm/elm')
require('codemirror/addon/lint/lint')
require('codemirror/addon/lint/lint.css')

module.exports = () =>
  Promise
    .resolve()
    .then(() => {
      window.CodeMirror = CodeMirror
    })
