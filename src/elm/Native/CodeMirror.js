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
      indentUnit: 4,
      indentWithTabs: false,
      tabSize: 4,
      lint: { lintOnChange: false },
      extraKeys: {
        Tab: function(cm) {
          var spaces = Array(cm.getOption("indentUnit") + 1).join(" ");
          cm.replaceSelection(spaces);
        }
      }
    })

    instance.__ellie_errors = [];

    var element = instance.getWrapperElement()

    Object.defineProperties(element, {
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
          return instance.getValue()
        },
        set: function (value) {
          if (value !== instance.getValue() && !element.__dispatchingChanges) {
            var prevScrollPosition = instance.getScrollInfo()
            instance.setValue(value)
            instance.scrollTo(prevScrollPosition.left, prevScrollPosition.top)
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

    var runDispatch = debounce(function () {
      var event = new Event('CodeMirror.updated')
      element.dispatchEvent(event)
    }, 30)

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
