module Cookie(get, set, Cookie, writeCookie, readCookie) where

{-| Everyone needs [cookies](http://www.quirksmode.org/js/cookies.html) sometimes.

  This library lets you get and set cookies, as Tasks or Effects. Effects are designed
  to work with the outside world, and cookies are part of the outside world.

# Type
@docs Cookie

# Effects
@docs writeCookie, readCookie

# Tasks
@docs get, set

 -}
import Task exposing (Task)
import Effects exposing (Effects)
import Native.Cookie

{-| Represents one cookie.
 -}
type alias Cookie = 
  {
    key: String,
    value: String
  }

{-| Create a task that sets a cookie. 
    Currently sets only the value, leaving it to expire when you close the browser.

    If successful, it returns a cookie with key and value populated.

    It checks to make sure the cookie was indeed set to the value you passed in. If not, it returns a failure description.
    Note: if you open a file:// in Chrome, cookies will not work. Use Firefox to test.
 -}
set: Cookie -> Task String Cookie
set = Native.Cookie.set

{-| Fetch the value of a cookie. If the cookie is not set, returns Nothing.

  There are no error conditions.
 -}
get: String -> Task Effects.Never (Maybe Cookie)
get = Native.Cookie.get

{-| Create an Effects Action that sets a cookie, where Action is your own type.

  Supply an Action constructor for errors (represented by a String)
  and an Action constructor for success (take a Cookie). The successful cookie will have key and value populated.
 -}
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

{-| Create an Effects Action to give you the value of a cookie, where Action is your own type.

  Supply an Action constructor for the Maybe Cookie that you get back.
  There is no failure case.
 -}
readCookie : (Maybe Cookie -> action) -> String -> Effects action
readCookie successConstructor key =
  get key 
  |> Task.map successConstructor
  |> Effects.task



