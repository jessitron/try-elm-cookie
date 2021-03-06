Elm.Native.Cookie = {};
Elm.Native.Cookie.make = function(localRuntime) {

  localRuntime.Native = localRuntime.Native || {};
  localRuntime.Native.Cookie = localRuntime.Native.Cookie || {};
  if (localRuntime.Native.Cookie.values)
  {
    return localRuntime.Native.Cookie.values;
  }

  var Task = Elm.Native.Task.make(localRuntime);
  var Maybe = Elm.Maybe.make(localRuntime);

  var howToGetACookie = function(sKey) {
    return decodeURIComponent(document.cookie.replace(new RegExp("(?:(?:^|.*;)\\s*" + encodeURIComponent(sKey).replace(/[\-\.\+\*]/g, "\\$&") + "\\s*\\=\\s*([^;]*).*$)|^.*$"), "$1")) || null;
  }

  function set(co) 
  {
    key = co.key
    value = co.value
    var setcommand = encodeURIComponent(key) + "=" + encodeURIComponent(value)
    document.cookie = setcommand
    var newValue = howToGetACookie(key)
    if (newValue === value)
    {
      return Task.succeed({key: key, value: value});  
    } else {
      return Task.fail("cookie " + setcommand + " was not set. It holds <" + newValue + ">")
    }
  }

  function get(key)
  {
    var value = howToGetACookie(key)
    if (!value) {
      return Task.succeed(Maybe.Nothing);
    } else {
      var output = {key: key, value: value};
      return Task.succeed(Maybe.Just(output));
    }
  }   

  return localRuntime.Native.Cookie.values = {
    set: set,
    get: get
  };

}