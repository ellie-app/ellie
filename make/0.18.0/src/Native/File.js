var _user$project$Native_File = {
  fromStringParts: F3(function(filename, mimeType, parts) {
    return new File(_elm_lang$core$Native_List.toArray(parts), filename, { type: mimeType })
  }),
  fileBody: function(x) {
    return { ctor: 'FormDataBody', _0: x }
  },
  filePart: F2(function (name, value) {
    return { ctor: 'StringPart', _0: name, _1: value }
  }),
  fromValue: function (value) {
    if (value instanceof File) {
      return { ctor: 'Ok', _0: value }
    } else {
      return { ctor: 'Err', _0: 'Value is not a File' }
    }
  },
  name: function (file) {
    return file.name
  },
  lastModified: function (file) {
    return file.lastModified
  },
  encoder: function (file) {
    return file
  },
  toObjectUrl: function (file) {
    return URL.createObjectURL(file)
  },
  expect: F2(function (filename, mimeType) {
    return {
      responseType: 'arraybuffer',
      responseToResult: function (response) {
        var file = new File([response.body], filename, { type: mimeType });
        return { ctor: 'Ok', _0: file }
      }
    }
  })
}
