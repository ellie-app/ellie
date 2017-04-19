require('./Polyfills')
var SourceLoader = require('./Compiler/SourceLoader')
var HtmlFixer = require('./Compiler/HtmlFixer')
var sha256 = require('js-sha256')
var Memoize = require('./Compiler/Memoize')

var compiler = null
var oldLog = console.log
var errorCallback = null
var originalCompileIfNeeded = null

function loadCompiler() {
  if (compiler) {
    return Promise.resolve(compiler)
  }

  return SourceLoader
    .load(__webpack_require__.p + 'Dll.ElmCompiler.js', function (event) {
      self.postMessage({ type: 'LoadingCompiler', percentage: event.loaded / event.total })
    })
    .then(function (init) {
      self.postMessage({ type: 'LoadingCompiler', percentage: 1 })
      return init()
    })
    .then(function (ElmCompiler) {
      compiler = ElmCompiler
      localStorage.setItem('apiKey', '28702525867ca365f0c0a62d65a54720541b8c9a')
      return compiler
    })
}

function patchCompile() {
  var toCompileSet = new Set()
  var compiledSet = new Set()
  originalCompileIfNeeded = compiler.ElmPackage.prototype._compileIfNeeded
  compiler.ElmPackage.prototype._compileIfNeeded = function (mods, i) {
    mods.forEach(function (mod) { toCompileSet.add(mod) })
    self.postMessage({ type: 'Compiling', total: toCompileSet.size, complete: compiledSet.size })
    return originalCompileIfNeeded.call(this, mods, i)
    .then(function (v) {
      mods.forEach(function (mod) { compiledSet.add(mod) })
      self.postMessage({ type: 'Compiling', total: toCompileSet.size, complete: compiledSet.size })
      return v
    })
  }
}

function unpatchCompile() {
  compiler.ElmPackage.prototype._compileIfNeeded = originalCompileIfNeeded
}

function patchLog() {
  console.log = function () {
    oldLog.apply(console, arguments)
    var stack = new Error().stack
    if (stack.indexOf('h$errorMsg') !== -1 && arguments[0].indexOf('elm-make') !== -1)
      errorCallback && errorCallback(Error(arguments[0]))
  }
}

function unpatchLog() {
  console.log = oldLog
}

var moduleRe = /^module ([a-zA-Z.]+) exposing/

function getModuleName(source) {
  var match = moduleRe.exec(source)
  return match && match[1] ? match[1] : null
}

function versionToString (version) {
  return version.major + '.' + version.minor + '.' + version.patch
}

function nextPatchVersion (version) {
  return {
    major: version.major,
    minor: version.minor,
    patch: version.patch + 1
  }
}

function packagesToElmPackage(packages) {
  var dependencies = packages.reduce(function (memo, pakage) {
    memo[pakage.username + '/' + pakage.name] = versionToString(pakage.version) + ' <= v < ' + versionToString(nextPatchVersion(pakage.version))
    return memo
  }, {})

  return {
    "version": "1.0.0",
    "source-directories": ["src"],
    "repository": "https://github.com/user/project.git",
    "exposed-modules": [],
    "dependencies": dependencies
  }
}

var execCompile = Memoize.memoizeAsync(
  function execCompile(elm, packages, compiler) {
    var moduleName = getModuleName(elm) || 'Main'
    var packageSpec = { user: "user", project: "project", version: "1.0.0" }

    var retriever = new compiler.GithubSource()
    retriever.useSourceFile(packageSpec, [moduleName], elm)
    retriever.useJson(packageSpec, packagesToElmPackage(packages))

    var elmPackage = new compiler.ElmPackage(retriever, packageSpec)

    return new Promise((resolve, reject) => {
      errorCallback = reject
      patchLog()
      patchCompile()
      elmPackage
        .expandPackage([moduleName])
        .then(function (reachable) {
          return elmPackage.compileModule(moduleName, true)
        })
        .then(function () {
          return elmPackage.link([moduleName])
        })
        .then(function (s) {
          var blob = new Blob([s], { type: 'application/javascript' })
          var url = URL.createObjectURL(blob)
          return {
            success: true,
            url: url
          }
        })
        .catch(function (e) {
          debugger
          return {
            success: false,
            errors: e.message.split('\n').map(function (v) { return JSON.parse(v) })
          }
        })
        .then(function (v) {
          unpatchLog()
          unpatchCompile()
          return v
        })
        .then(resolve)
        .catch(reject)
    })
  },
  function(elm) {
    return sha256(elm)
  }
)

var buildHtml = Memoize.memoizeAsync(
  function (html, elmUrl) {
    return HtmlFixer
      .fix({
        embedApi: true,
        htmlCode: html,
        sourceScript: '<script src="' + elmUrl + '"></script>'
      })
  },
  function (html, elmUrl) {
    return sha256(html) + elmUrl
  }
)

function compile(html, elm, packages) {
  loadCompiler()
  .then(function (compiler) {
    return execCompile(elm, packages, compiler)
      .then(function (result) {
        if (!result.success) {
          return { type: 'FinishedWithErrors', errors: result.errors }
        }

        return buildHtml(html, result.url)
          .then(function (htmlUrl) {
            return { type: 'Success', url: htmlUrl }
          })
      })
      .then(function (message) {
        self.postMessage(message)
      })
      .catch(function (error) {
        debugger
        self.postMessage({
          type: 'Failed',
          message: error.message
        })
      })
  })
}

self.addEventListener('message', function (event) {
  compile(event.data.html, event.data.elm, event.data.packages)
})
