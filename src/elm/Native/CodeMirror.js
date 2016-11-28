var _user$project$Native_CodeMirror = (function () {
  var Nil = _elm_lang$core$Native_List.Nil
  var CodeMirror = window.CodeMirror

  function editor(factList) {
    return _elm_lang$virtual_dom$Native_VirtualDom.custom(factList, null, implementation)
  }

  var implementation = {
    render: render,
    diff: diff
  }

  function render() {
    var instance = CodeMirror(null, {
      lineNumbers: true
    })

    var element = instance.getWrapperElement()

    console.log('element', element)

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

  return {
    editor: editor
  }
}())
