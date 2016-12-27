var _user$project$Native_ClickOutside = (function () {

  function onClickOutside(groups) {
    var array = _elm_lang$core$Native_List.toArray(groups)
    var map = array.reduce(function (memo, tuple) {
      memo[tuple._0] = tuple._1
      return memo
    }, {})

    return _elm_lang$core$Native_Scheduler.nativeBinding(function (callback) {
      function onClick(event) {
        Object.keys(map).forEach(function (id) {
          var el = document.getElementById(id)
          if (el && !el.contains(event.target)) {
            _elm_lang$core$Native_Scheduler.rawSpawn(map[id])
          }
        })
      }

      document.addEventListener('click', onClick)

      return function () {
        document.removeEventListener('click', onClick)
      }
    })
  }


  return {
    onClickOutside: onClickOutside
  }
}())
