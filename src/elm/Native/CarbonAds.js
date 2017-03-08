var _user$project$Native_CarbonAds = (function () {
  var Nil = _elm_lang$core$Native_List.Nil

  function ad(zoneId, serve, placement) {
    var model = {
      zoneId: zoneId,
      serve: serve,
      placement: placement
    }

    return _elm_lang$virtual_dom$Native_VirtualDom.custom(Nil, model, implementation)
  }

  var implementation = {
    render: render,
    diff: diff
  }

  function render(model) {
    var element = document.createElement('div')
    element.id = 'carbon'

    var script = document.createElement('script')
    script.async = true
    script.src = '//cdn.carbonads.com/carbon.js?zoneid=' + model.zoneId + '&serve=' + model.serve + '&placement=' + model.placement
    script.id = '_carbonads_js'
    script.type = 'text/javascript'

    element.appendChild(script)

    return element
  }

  function diff(a, b) {
    return null
  }

  return {
    ad: F3(ad)
  }
}())
