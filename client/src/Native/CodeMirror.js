var _user$project$Native_CodeMirror = (function () {
  var Nil = _elm_lang$core$Native_List.Nil
  var toArray = _elm_lang$core$Native_List.toArray
  var CodeMirror = window.CodeMirror

  function debounce(func, wait) {
  	var timeout
  	return function() {
  		var later = function() {
  			timeout = null
  			func.apply(null, arguments)
  		};
  		clearTimeout(timeout)
  		timeout = setTimeout(later, wait);
  	}
  }

  CodeMirror.registerHelper('lint', 'elm', function (text, options, instance) {
    return instance.__ellie_errors || []
  })

  function editor(factList) {
    return _elm_lang$virtual_dom$Native_VirtualDom.custom(factList, null, implementation)
  }

  var implementation = {
    render: render,
    diff: diff
  }

  function render() {
    var instance = CodeMirror(null, {
      lineNumbers: true,
      styleActiveLine: { nonEmpty: true },
      smartIndent: true,
      indentWithTabs: false,
      indentWidth: 2,
      tabSize: 2,
      lint: { lintOnChange: false },
      keyMap: 'default',
      extraKeys: {
        Tab: function(cm) {
          var x = ""
          for (var i = cm.getOption('indentUnit'); i > 0; i--) {
            x += " "
          }
          cm.replaceSelection(x);
        }
      }
    })

    instance.__ellie_errors = [];

    var element = instance.getWrapperElement()

    Object.defineProperties(element, {
      vimMode: {
        get: function () {
          return instance.getOption('keyMap') === 'vim'
        },
        set: function (value) {
          instance.setOption('keyMap', value ? 'vim' : 'default')
        }
      },
      indentWidth: {
        get: function () {
          return instance.getOption('indentUnit')
        },
        set: function (value) {
          instance.setOption('indentUnit', value)
          instance.setOption('tabSize', value)
        }
      },
      readOnly: {
        get: function () {
          return instance.getOption('readOnly')
        },
        set: function (value) {
          instance.setOption('readOnly', value)
        }
      },
      editorValue: {
        get: function () {
          return element.__debouncedValue
        },
        set: function (value) {
          if (value !== element.__debouncedValue) {
            var prevScrollPosition = instance.getScrollInfo()
            instance.setValue(value)
            instance.scrollTo(prevScrollPosition.left, prevScrollPosition.top)
            element.__debouncedValue = value
          }
        }
      },
      mode: {
        get: function () {
          return instance.getOption('mode')
        },
        set: function (value) {
          instance.setOption('mode', value)
        }
      },
      theme: {
        get: function () {
          return instance.getOption('theme')
        },
        set: function (value) {
          instance.setOption('theme', value)
        }
      },
      linterMessages: {
        set: function (errors) {
          if (!errors) {
            instance.__ellie_errors = []
            return
          }

          instance.__ellie_errors = errors.map(function (i) {
            var div = document.createElement('div')
            div.innerHTML = i.message
            return {
              from: i.from,
              to: i.to,
              message: div.innerText,
              severity: i.severity
            }
          })
          instance.performLint()
        },
        get: function () {
          return instance.__ellie_errors
        }
      }
    })

    element.__debouncedValue = instance.getValue()

    var runDispatch = debounce(function () {
      element.__debouncedValue = instance.getValue()
      var event = new CustomEvent('CodeMirror.updated')
      element.dispatchEvent(event)
    }, 200)

    instance.on('change', runDispatch)

    requestAnimationFrame(function () {
      instance.refresh()
    })

    element.__CODE_MIRROR_INSTANCE_ELM__ = instance
    return element
  }

  function diff(a, b) {
    return null
  }

  function position(line, column) {
    return CodeMirror.Pos(line, column)
  }

  function linterMessage(severity, message, from, to) {
    return {
      from: from,
      to: to,
      message: message,
      severity: severity
    }
  }

  function encodeLinterMessage(message) {
    return message;
  }

  return {
    editor: editor,
    position: F2(position),
    linterMessage: F4(linterMessage),
    encodeLinterMessage: encodeLinterMessage
  }
}())
