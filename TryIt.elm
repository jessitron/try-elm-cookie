module TryIt where

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Signal exposing (Address)
import Effects exposing (Effects, Never)
import Task exposing (Task)
import Cookie exposing (Cookie)
import StartApp

app = StartApp.start 
  {
  init = (init, readMyCookie cookieKey),
  update = update,
  view = view,
  inputs = []
  }

main = app.html

port tasks : Signal (Task.Task Never ())
port tasks =
    app.tasks


-- MODEL
type alias Model = {
  input: String,
  cookie: Maybe String,
  setCount: Int
}

cookieKey = "potato"
init = {
  input = "",
  cookie = Nothing,
  setCount = 0
 }

-- VIEW
view address model = Html.div [Attr.style [("padding", "50px")]]
       [
         divc (Html.input [Attr.value model.input,
                     Events.on "input" Events.targetValue (Signal.message address << Input)] []),
         divc( Html.text ("The cookie contains: " ++ (Maybe.withDefault "--" model.cookie))),
         divc(Html.text ("set " ++ (toString model.setCount) ++ " times"))

       ]

divc content = Html.div [Attr.style [("padding", "5px")]] [content]

-- UPDATE
type Action = 
    Input String
  | Cookie (Maybe Cookie)
  | SetOk
  | Failure String

update action model = 
  case action of
    Input str -> ({model | input <- str}, writeMyCookie str )
    SetOk -> ({model | setCount <- (model.setCount + 1)}, readMyCookie cookieKey)
    Failure boo -> ({model | cookie <- Just ("FAILURE: " ++ boo)}, Effects.none)
    Cookie c -> ({model | cookie <- (Maybe.map .value c)}, Effects.none)

writeMyCookie: String -> Effects Action
writeMyCookie str = writeCookie Failure (\_ -> SetOk) { key = cookieKey, value = str }

readMyCookie: String -> Effects Action
readMyCookie = readCookie (\a -> Failure ("while reading: " ++ a)) Cookie

writeCookie : (String -> action) -> (Cookie -> action) -> Cookie -> Effects action
writeCookie failureConstructor successConstructor cookie =
  let
    interpreter result = 
      case result of
        Ok ok   -> successConstructor ok
        Err err -> failureConstructor err
  in
  Cookie.set cookie
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
  Cookie.get key 
  |> Task.toResult
  |> Task.map interpreter
  |> Effects.task


