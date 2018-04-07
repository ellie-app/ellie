export default {
  flags(current) {
    return Object.assign({}, current, {
      token: localStorage.getItem('Pages.Editor.token')
    })
  },
  start(app) {

    let hasUnsavedWork = false;
    let previousLocation = window.location.pathname
    window.addEventListener('popstate', e => {
      if (hasUnsavedWork) {
        const result = window.confirm('You have unsaved work. Are you sure you want to go?')
        if (!result) {
          window.history.pushState({}, '', previousLocation)
          e.preventDefault()
        }
      }
    })
  }
}
