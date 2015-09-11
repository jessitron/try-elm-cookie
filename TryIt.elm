module TryIt where

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Signal exposing (Address)
import Effects exposing (Effects, Never)
import Task exposing (Task)
import Cookie
import StartApp

app = StartApp.start 
  {
  init = (init, readCookie ()),
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
view address model = Html.div []
       [
         divc (Html.input [Attr.value model.input,
                     Events.on "input" Events.targetValue (Signal.message address << Input)] []),
         divc( Html.text (Maybe.withDefault "--" model.cookie)),
         divc(Html.text ("set " ++ (toString model.setCount) ++ " times"))

       ]

divc content = Html.div [] [content]

-- UPDATE
type Action = 
    Input String
  | Cookie (Maybe String)
  | SetOk
  | Failure String

update action model = 
  case action of
    Input str -> ({model | input <- str}, writeCookie str )
    SetOk -> ({model | setCount <- (model.setCount + 1)}, readCookie ())
    Failure boo -> ({model | cookie <- Just ("FAILURE: " ++ boo)}, Effects.none)
    Cookie c -> ({model | cookie <- c}, Effects.none)

writeCookie : String -> Effects Action
writeCookie str =
  Cookie.set cookieKey str
  |> Task.toResult
  |> Task.map hooray
  |> Effects.task

hooray result =
  case result of
    Ok butt  -> SetOk
    Err face -> Failure face

readCookie : () -> Effects Action
readCookie _ =
  Cookie.get cookieKey
  |> Task.toResult
  |> Task.map huzzah
  |> Effects.task

huzzah result =
  case result of
    Ok butt  -> Cookie butt
    Err face -> Failure (" while reading! " ++ face)


