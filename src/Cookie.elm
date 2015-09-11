module Cookie(get, set, Cookie) where

{-| Everyone needs cookies sometime

@docs get, set

 -}
import Task exposing (Task)
import Native.Cookie

type alias Cookie = 
  {
    k: String,
    v: String
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