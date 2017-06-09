export default function (vimMode) {
  return Promise.all([
    import('codemirror/lib/codemirror'),
    import('codemirror/lib/codemirror.css'),
    import('codemirror/mode/elm/elm'),
    import('codemirror/mode/htmlmixed/htmlmixed'),
    import('codemirror/addon/lint/lint'),
    import('codemirror/addon/selection/active-line'),
    import('codemirror/addon/lint/lint.css'),
    import('codemirror/theme/material.css'),
    import('./CodeMirror.css')
  ]).then(([CodeMirror]) => {
    window.CodeMirror = CodeMirror

    if (!vimMode) return

    return Promise.all([
      import('codemirror/keymap/vim'),
      import('codemirror/addon/dialog/dialog'),
      import('codemirror/addon/dialog/dialog.css')
    ]).then(() => {})
  })
}
