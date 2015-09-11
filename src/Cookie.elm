module Cookie(get, set, Cookie, writeCookie, readCookie) where

{-| Everyone needs cookies sometime

@docs get, set

 -}
import Task exposing (Task)
import Effects exposing (Effects)
import Native.Cookie

type alias Cookie = 
  {
    key: String,
    value: String
  }

{-| Set a cookie. 
    Currently sets only the value, leaving it to expire when you close the browser.

    set key value

    TODO: error constructor?
 -}
set: Cookie -> Task String Cookie
set = Native.Cookie.set

get: String -> Task String (Maybe Cookie)
get = Native.Cookie.get


writeCookie : (String -> action) -> (Cookie -> action) -> Cookie -> Effects action
writeCookie failureConstructor successConstructor cookie =
  let
    interpreter result = 
      case result of
        Ok ok   -> successConstructor ok
        Err err -> failureConstructor err
  in
  set cookie
  |> Task.toResult
  |> Task.map interpreter
  |> Effects.task

readCookie : (String -> action) -> (Maybe Cookie -> action) -> String -> Effects action
readCookie failureConstructor successConstructor key =
  let
    interpreter result = 
      case result of
        Ok ok   -> successConstructor ok
        Err err -> failureConstructor err
  in
  get key 
  |> Task.toResult
  |> Task.map interpreter
  |> Effects.task