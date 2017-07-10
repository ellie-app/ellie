export default function (vimMode) {
  return Promise.all([
    import(/* webpackChunkName: "codemirror-base" */ 'codemirror/lib/codemirror'),
    import(/* webpackChunkName: "codemirror-base", webpackMode: "eager" */ 'codemirror/lib/codemirror.css'),
    import(/* webpackChunkName: "codemirror-base" */ 'codemirror/mode/elm/elm'),
    import(/* webpackChunkName: "codemirror-base" */ 'codemirror/mode/htmlmixed/htmlmixed'),
    import(/* webpackChunkName: "codemirror-base" */ 'codemirror/addon/lint/lint'),
    import(/* webpackChunkName: "codemirror-base" */ 'codemirror/addon/selection/active-line'),
    import(/* webpackChunkName: "codemirror-base", webpackMode: "eager" */ 'codemirror/addon/lint/lint.css'),
    import(/* webpackChunkName: "codemirror-base", webpackMode: "eager" */ 'codemirror/theme/material.css'),
    import(/* webpackChunkName: "codemirror-base", webpackMode: "eager" */ './CodeMirror.css')
  ]).then(([CodeMirror]) => {
    window.CodeMirror = CodeMirror

    if (!vimMode) return

    return Promise.all([
      import(/* webpackChunkName: "codemirror-vim" */ 'codemirror/keymap/vim'),
      import(/* webpackChunkName: "codemirror-vim" */ 'codemirror/addon/dialog/dialog'),
      import(/* webpackChunkName: "codemirror-vim", webpackMode: "eager" */ 'codemirror/addon/dialog/dialog.css')
    ]).then(() => {})
  })
}
