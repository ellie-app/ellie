import { requestIdleCallback, cancelIdleCallback } from 'request-idle-callback'

const getToken = (editor) => {
  const position = editor.getCursor()
  const line = position.line
  let token = editor.getTokenAt(position)
  if (!token.type) {
    token = editor.getTokenAt({ line: line, ch: position.ch + 1 })
  }
  if (token.type === 'variable' || token.type === 'variable-2' || token.type === 'type') {
    return expandLeft(editor, line, token.start, token.string);
  }
  if (token.string === '.' || token.type === 'qualifier') {
    return expandRight(editor, line, token.end, expandLeft(editor, line, token.start, token.string))
  }
  if (token.type === 'builtin') {
    return token.string
  }
  return null
}

const expandLeft = (editor, line, start, string) => {
  const token = editor.getTokenAt({ line: line, ch: start })
  if (start === token.start) {
    return string
  }
  if (token.string === '.' || token.type === 'qualifier') {
    return expandLeft(editor, line, token.start - 1, token.string + string)
  }
  return string
}

const expandRight = (editor, line, end, string) => {
  const token = editor.getTokenAt({ line: line, ch: end + 1 })
  if (end === token.end) {
    return string
  }
  if (token.string === '.' || token.type === 'qualifier') {
    return expandRight(editor, line, token.end, string + token.string)
  }
  if (token.type === 'variable' || token.type === 'variable-2' || token.type === 'type') {
    return string + token.string
  }
  return string
}

const load = () => {
  return Promise.all([
    import(/* webpackChunkName: "codemirror-base" */ 'codemirror/lib/codemirror'),
    import(/* webpackChunkName: "codemirror-base", webpackMode: "eager" */ 'codemirror/lib/codemirror.css'),
    import(/* webpackChunkName: "codemirror-base" */ 'codemirror/mode/elm/elm'),
    import(/* webpackChunkName: "codemirror-base" */ 'codemirror/mode/htmlmixed/htmlmixed'),
    import(/* webpackChunkName: "codemirror-base" */ 'codemirror/addon/lint/lint'),
    import(/* webpackChunkName: "codemirror-base" */ 'codemirror/addon/selection/active-line'),
    import(/* webpackChunkName: "codemirror-base", webpackMode: "eager" */ 'codemirror/addon/lint/lint.css'),
    import(/* webpackChunkName: "codemirror-base", webpackMode: "eager" */ 'codemirror/theme/material.css'),
    import(/* webpackChunkName: "codemirror-base", webpackMode: "eager" */ './CodeEditor.css')
  ])
  .then(([CodeMirror]) => CodeMirror)
}

const loadVimMode = () => {
  return Promise.all([
    import(/* webpackChunkName: "codemirror-vim" */ 'codemirror/keymap/vim'),
    import(/* webpackChunkName: "codemirror-vim" */ 'codemirror/addon/dialog/dialog'),
    import(/* webpackChunkName: "codemirror-vim", webpackMode: "eager" */ 'codemirror/addon/dialog/dialog.css')
  ])
  .then(() => {})
}

const debounce = (func) => {
  let token
  return function() {
    const later = () => {
      token = null
      func.apply(null, arguments)
    };
    cancelIdleCallback(token)
    token = requestIdleCallback(later)
  }
}

const STALE_TOKEN = {}

const start = (app) => {
  return load().then(CodeMirror => {
    CodeMirror.registerHelper('lint', 'elm', (text, options, instance) => {
      return instance._errors || []
    })

    customElements.define('code-editor', class CodeEditor extends HTMLElement {
      constructor() {
        super()
        this._value = this.editorValue || ''
        delete this.editorValue

        this._tabSize = this.tabSize || 4
        delete this.tabSize

        this._readOnly = this.readOnly || false
        delete this.readOnly

        this._mode = this.mode || 'htmlmixed'
        delete this.mode

        this._onCursorActivity = this._onCursorActivity.bind(this)
        this._onElmChanges = this._onElmChanges.bind(this)
        this._ready = false
        this._instance = null
        this._errors = []
        this._vimMode = false
        this._vimModeLoadState = 'NOT_ASKED'
        this._idleCallback = null
        this._token = STALE_TOKEN
      }

      get vimMode() {
        return this._vimMode
      }
      set vimMode(value) {
        if (value === null) value = false
        if (value === this._vimMode) return
        this._vimMode = value
        if (this._vimModeLoadState === 'NOT_ASKED' && this._vimMode) {
          this._vimModeLoadState = 'LOADING'
          loadVimMode().then(() => {
            this._vimModeLoadState = 'FINISHED'
            if (!this._instance) return
            this._instance.setOption('keyMap', this._vimMode ? 'vim' : 'default')
          })
        } else if (this._vimModeLoadState === 'FINISHED' && this._instance) {
          this._instance.setOption('keyMap', this._vimMode ? 'vim' : 'default')
        }
      }

      get editorValue() {
        return this._value
      }
      set editorValue(value) {
        if (value !== null && value !== this._value) {
          this._value = value
          if (!this._instance) return
          const prevScrollPosition = this._instance.getScrollInfo()
          this._instance.setValue(value)
          this._instance.scrollTo(prevScrollPosition.left, prevScrollPosition.top)
        }
      }

      get tabSize() {
        return this._tabSize
      }
      set tabSize(value) {
        if (value === null) value = 4
        if (value === this._tabSize) return
        this._tabSize = value
        if (!this._instance) return
        this._instance.setOption('indentWidth', this._tabSize)
        this._instance.setOption('tabSize', this._tabSize)
        this._instance.setOption('indentUnit', this._tabSize)
      }

      get readOnly() {
        return this._readOnly
      }
      set readOnly(value) {
        if (value === null) value = false
        if (value === this._readOnly) return
        this._readOnly = value
        if (!this._instance) return
        this._instance.setOption('readOnly', value)
      }

      get mode() {
        return this._mode
      }
      set mode(value) {
        if (value === null) value = 'htmlmixed'
        if (value === this._mode) return
        this._mode = value
        if (!this._instance) return
        this._instance.setOption('mode', this._mode)
        if (value === 'elm') {
          this._setupElmEvents()
        } else {
          this._tearDownElmEvents()
        }
      }

      get linterMessages() {
        return this._errors
      }
      set linterMessages(value) {
        if (value === null) value = []
        this._errors = value
        if (!this._instance) return
        this._instance._errors = this.formatLinterMessages(value)
        this._instance.performLint()
      }

      get token() {
        if (!this._instance) return null
        if (this._token === STALE_TOKEN) this._token = getToken(this._instance)
        return this._token
      }

      moveCursor(line, column) {
        if (!this._instance) return
        this._instance.focus()
        this._instance.setCursor({ line: line - 1, ch: column - 1 })
      }

      connectedCallback() {
        if (this._instance) return
        this._instance = CodeMirror(this, {
          lineNumbers: true,
          styleActiveLine: { nonEmpty: true },
          smartIndent: true,
          indentWithTabs: false,
          keyMap: this._vimModeLoadState === 'FINISHED' && this._vimMode ? 'vim' : 'default',
          lint: { lintOnChange: false },
          theme: 'material',
          indentWidth: this._tabSize,
          tabSize: this._tabSize,
          indentUnit: this._tabSize,
          readOnly: this._readOnly,
          mode: this._mode,
          value: this._value,
          dragDrop: false,
          extraKeys: {
            Tab(cm) {
              let x = ""
              for (let i = cm.getOption('indentUnit'); i > 0; i--) x += " "
              cm.replaceSelection(x)
            }
          }
        })

        const runDispatch = debounce(() => {
          this._value = this._instance.getValue()
          const event = new Event('change')
          this.dispatchEvent(event)
        })

        this._instance.on('changes', runDispatch)
      
        if (this._mode === 'elm') this._setupElmEvents()

        requestAnimationFrame(() => {
          this._instance.refresh()
        })

        if (this._vimMode && this._vimModeLoadState !== 'FINISHED') {
          this._vimModeLoadState = 'LOADING'
          loadVimMode().then(() => {
            this._vimModeLoadState = 'FINISHED'
            this._instance.setOption('keyMap', this._vimMode ? 'vim' : 'default')
          })
        }
      }

      _setupElmEvents() {
        this._instance.on('cursorActivity', this._onCursorActivity)
        this._instance.on('changes', this._onElmChanges)
      }

      _tearDownElmEvents() {
        this._instance.off('cursorActivity', this._onCursorActivity)
        this._instance.off('cursorActivity', this._onCursorActivity)
      }

      _onCursorActivity() {
        this._token = STALE_TOKEN
        this._idleCallback = requestIdleCallback(() => {
          this._idleCallback = null
          this.dispatchEvent(new CustomEvent('settled'))
        })
      }

      _onElmChanges() {
      }

      formatLinterMessages(messages) {
        return messages.map(message => {
          return {
            from: message.from,
            to: message.to,
            message: message.message,
            severity: message.severity
          }
        })
      }
    })
  })
}

export default { start }
