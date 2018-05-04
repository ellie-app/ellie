const notWordRegex = /^\W/

export const getCompletionContext = (editor) => {
  const position = editor.getCursor()
  const line = position.line
  
  let token = editor.getTokenAt(position)
  if (!token.type) {
    token = editor.getTokenAt({ line: line, ch: position.ch + 1 })
  }
  
  // The cursor is next to the dot at the end of a module name
  if (token.type === 'qualifier' && position.ch === token.end) {
    const preceding = getQualifierFor(editor, line, token.start)
    return {
      type: 'Qualifier',
      from: { line: position.line, ch: token.end },
      to: { line: position.line, ch: token.end },
      text: preceding === null ? token.string.replace('.', '') : preceding + '.' + token.string.replace('.', '')
    }
  }

  if (token.type === 'builtin' && notWordRegex.test(token.string)) {
    return {
      type: 'Operator',
      from: { line: position.line, ch: token.start },
      to: { line: position.line, ch: token.end },
      text: token.string
    }
  }

  // The cursor is in a lowercase variable
  if (token.type === 'variable') {
    return {
      type: 'LowercaseVar',
      text: token.string,
      from: { line: position.line, ch: token.start },
      to: { line: position.line, ch: token.end },
      qualifier: getQualifierFor(editor, line, token.start)  
    }
  }

  // The cursor is in an uppercase variable
  if (token.type === 'variable-2') {
    const qualifier = getQualifierFor(editor, line, token.start)
    return {
      type: 'UppercaseVar',
      text: token.string,
      from: { line: position.line, ch: token.start },
      to: { line: position.line, ch: token.end },
      qualifier: getQualifierFor(editor, line, token.start)
    }
  }

  return {
    type: 'Unknown',
    from: { line: position.line, ch: token.start },
    to: { line: position.line, ch: token.end }
  }
}

const getQualifierFor = (editor, line, start) => {
  const token = editor.getTokenAt({ line: line, ch: start })
  if (token.type === 'qualifier') {
    const preceding = getQualifierFor(editor, line, token.start)
    if (preceding === null) return token.string.replace('.', '')
    else return preceding + '.' + token.string.replace('.', '')
  }

  return null
}
