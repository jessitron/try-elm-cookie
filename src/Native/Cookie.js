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

  console.log("what it is here " + localRuntime);

  console.log("what is the document " + document);
  console.log("what is the document cookie <" + document.cookie + ">");


  var howToGetACookie = function(sKey) {
    return decodeURIComponent(document.cookie.replace(new RegExp("(?:(?:^|.*;)\\s*" + encodeURIComponent(sKey).replace(/[\-\.\+\*]/g, "\\$&") + "\\s*\\=\\s*([^;]*).*$)|^.*$"), "$1")) || null;
  }

  function set(key, value) 
  {   
    return Task.asyncFunction(function(callback) {
      console.log("set triggered")
      var setcommand = encodeURIComponent(key) + "=" + encodeURIComponent(value)
      document.cookie = setcommand
      var newValue = howToGetACookie(key)
      if (newValue === value)
      {
        callback(Task.succeed({key: key, value: value}));  
      } else {
        callback(Task.fail("cookie " + setcommand + " was not set. It holds <" + newValue + ">"));
      }
    });
   
  }

  function get(key)
  {
    return Task.asyncFunction(function(callback) {
      console.log ("get triggered")
      var value = howToGetACookie(key)
      if (!value) {
        return Task.succeed(Maybe.Nothing);
      } else {
        var output = {key: key, value: value};
        console.log("returning: " + output)
        console.log("returning: k" + output.key)
        console.log("returning: v" + output.value)
        callback(Task.succeed(Maybe.Just({key: key, value: value})));
      }
    });
  }   

  return localRuntime.Native.Cookie.values = {
    set: F2(set),
    get: get
  };

}