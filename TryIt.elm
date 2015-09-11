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
  init = (init, readCookie cookieKey),
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
    Input str -> ({model | input <- str}, writeCookie Failure (\_ -> SetOk) { key = cookieKey, value = str } )
    SetOk -> ({model | setCount <- (model.setCount + 1)}, readCookie cookieKey)
    Failure boo -> ({model | cookie <- Just ("FAILURE: " ++ boo)}, Effects.none)
    Cookie c -> ({model | cookie <- (Maybe.map .value c)}, Effects.none)

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

hooray result =
  case result of
    Ok butt  -> SetOk
    Err face -> Failure face

readCookie : String -> Effects Action
readCookie key =
  Cookie.get key 
  |> Task.toResult
  |> Task.map huzzah
  |> Effects.task

huzzah result =
  case result of
    Ok butt  -> Cookie butt
    Err face -> Failure (" while reading! " ++ face)


