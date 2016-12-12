var _user$project$Native_CodeMirror = (function () {
  var Nil = _elm_lang$core$Native_List.Nil
  var toArray = _elm_lang$core$Native_List.toArray
  var CodeMirror = window.CodeMirror

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
      styleActiveLine: true,
      lint: { lintOnChange: false }
    })

    instance.__ellie_errors = [];

    var element = instance.getWrapperElement()

    Object.defineProperties(element, {
      value: {
        get: function () {
          return instance.getValue()
        },
        set: function (value) {
          if (value !== instance.getValue()) {
            instance.setValue(value)
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
          instance.__ellie_errors = errors
          instance.performLint()
        },
        get: function () {
          return instance.__ellie_errors
        }
      }
    })

    instance.on('changes', function () {
      var event = new Event('CodeMirror.updated')
      element.dispatchEvent(event)
    })

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
