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
  init = ({}, writeCookie),
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
}

cookieKey = "potato"
init = {
 }

-- VIEW
view address model = Html.div []
       [  ]

-- UPDATE
type alias Action = Result String Cookie

update action model = (model, Effects.none)

writeCookie : Effects Action
writeCookie =
  Cookie.set cookieKey "value-o"
  |> Task.toResult
  |> Effects.task



