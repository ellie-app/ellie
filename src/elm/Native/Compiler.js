var _user$project$Native_Compiler = (function () {

  var parseDependencies = F2(function (p, source) {
    return _elm_lang$core$Native_Scheduler.nativeBinding(function (callback) {
      var packageArray = [p.user, p.project]

      self.ElmCompiler
        .parse(packageArray, source)
        .then(function (result) {
          var moduleName = _elm_lang$core$Native_List.fromArray(result[0])
          var imports = _elm_lang$core$Native_List.fromArray(result[1].map(_elm_lang$core$Native_List.fromArray))
          callback(_elm_lang$core$Native_Scheduler.succeed({ _0: moduleName, _1: imports }))
        })
        .catch(function (error) {
          callback(_elm_lang$core$Native_Scheduler.fail(_elm_lang$core$Native_List.fromArray([error.message])))
        })
    })
  })


  var compile = F4(function (name, isExposed, source, interfaces) {
    return _elm_lang$core$Native_Scheduler.nativeBinding(function (callback) {
      var nameJs = [name.user, name.project]
      var interfacesJs = _elm_lang$core$Native_List.toArray(interfaces)
        .map(function (tuple /* ({ package, name }, Interface) */) {
          return [
              [
                [
                  [ tuple._0.$package._0.user, tuple._0.$package._0.project ],
                  _elm_lang$core$Native_List.toArray(tuple._0.name)
                ],
                tuple._0.$package._1.major + '.' + tuple._0.$package._1.minor + '.' + tuple._0.$package._1.patch
              ],
              tuple._1._0
          ]
        })

      self.ElmCompiler
        .compile(nameJs, isExposed, source, interfacesJs)
        .then(function (result) {
          var success = result[0] !== 'false'
          var errorsOrInterface = result[1]
          var elmo = result[2]
          if (success) {
            callback(_elm_lang$core$Native_Scheduler.succeed({
              _0: { ctor: 'Interface', _0: errorsOrInterface },
              _1: elmo
            }))
          } else {
            var parsedErrors
            try {
              parsedErrors = errorsOrInterface.map(function (e) {
                return JSON.parse(e)
              })
            } catch (e) {
              parsedErrors = [
                {
                  tag: 'UNKNOWN',
                  overview: 'Compilation Failed',
                  details: errorsOrInterface[0] || e.message,
                  region: { start: { line: 0, column: 0 }, end: { line: 0, column: 0 } },
                  type: 'error'
                }
              ]
            }

            callback(_elm_lang$core$Native_Scheduler.fail(parsedErrors))
          }
        })
        .catch(function (error) {
          callback(_elm_lang$core$Native_Scheduler.fail([error.message]))
        })
      })
  })

  return {
    parseDependencies: parseDependencies,
    compile: compile
  }
}())
