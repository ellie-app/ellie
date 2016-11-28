var _user$project$Native_Mui = (function () {
  function button(factList, text) {
    return _elm_lang$virtual_dom$Native_VirtualDom.custom(factList, text, implementation)
  }

  var implementation = {
  	render: render,
  	diff: diff
  }

  function render(text) {
  	var button = document.createElement('button')
    button.innerHTML = text
    button.className += 'mui-btn'
  	return button
  }

  function diff(a, b) {
    if (a.model === b.model) {
      return null
    }

    return {
      applyPatch: applyPatch,
      data: b.model
    }
  }

  function applyPatch(domNode, data) {
    domNode.innerHTML = data
  	return domNode
  }

  return {
    button: F2(button)
  }
}())
