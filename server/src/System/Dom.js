var jsdom = require('jsdom')

exports._parseFromString = function _parse(input) {
  return function () {
    var dom = new jsdom.JSDOM(input)
    return dom.window.document
  }
}

// DOCUMENT

exports._documentQuerySelectorAll = function _documentQuerySelectorAll(inputs) {
  return function () {
    return Array.from(inputs.document.querySelectorAll(inputs.selector))
  }
}

exports._documentCreateElement = function _documentCreateElement(inputs) {
  return function () {
    return inputs.document.createElement(inputs.tagName)
  }
}

exports._documentPrepend = function _documentPrepend(inputs) {
  return function () {
    inputs.document.prepend(inputs.child)
    return inputs.helpers.unit
  }
}

exports._documentHead = function _documentHead(document) {
  return function () {
    return document.head
  }
}

exports._documentBody = function _documentBody(document) {
  return function () {
    return document.body
  }
}

exports._documentDocumentElement = function _documentDocumentElement(document) {
  return function () {
    return document.documentElement
  }
}

// ELEMENT

exports._elementGetAttribute = function (inputs) {
  return function () {
    var out = inputs.element.getAttribute(inputs.name)
    if (typeof out !== 'string') return inputs.helpers.nothing
    else return inputs.helpers.just(out)
  }
}

exports._elementSetAttribute = function (inputs) {
  return function () {
    inputs.element.setAttribute(inputs.name, inputs.value)
    return inputs.helpers.unit
  }
}

exports._elementGetTextContent = function (element) {
  return function () {
    return element.textContent
  }
}

exports._elementSetTextContent = function (inputs) {
  return function () {
    inputs.element.textContent = inputs.content
    return inputs.helpers.unit
  }
}

exports._elementSetInnerHtml = function (inputs) {
  return function () {
    inputs.element.innerHTML = inputs.html
    return inputs.helpers.unit
  }
}

exports._elementPrepend = function _elementPrepend(inputs) {
  return function () {
    inputs.parent.prepend(inputs.child)
    return inputs.helpers.unit
  }
}

exports._elementQuerySelectorAll = function _elementQuerySelectorAll(inputs) {
  return function () {
    return Array.from(inputs.element.querySelectorAll(inputs.selector))
  }
}

exports._elementGetOuterHtml = function _elementGetOuterHtml(element) {
  return function () {
    return element.outerHTML
  }
}

exports._elementAppendChild = function _elementAppendChild(inputs) {
  return function () {
    inputs.parent.appendChild(inputs.child)
    return inputs.helpers.unit
  }
}
