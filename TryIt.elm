module TryIt where

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Signal
import Effects exposing (Effects, Never)
import Task exposing (Task)
import Cookie exposing (writeCookie, readCookie)
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
port tasks = app.tasks


-- MODEL
type alias Model = 
  {
    input: String,
    cookie: Maybe String,
    setCount: Int
  }

cookieKey = "potato"
init = 
  {
    input = "",
    cookie = Nothing,
    setCount = 0
  }

-- VIEW
view address model = 
  Html.div 
    [ padding 50 ]
    [
      divc (
             Html.input 
               [
                 Attr.value model.input,
                 Events.on "input" Events.targetValue (Signal.message address << Input)
               ]
               []
           ),
      divc (displayCookieText model),
      divc (displayCountText model)
    ]

displayCookieText model = 
  Html.text ("The cookie contains: " ++ (Maybe.withDefault "--" model.cookie))

displayCountText model = Html.text ("set " ++ (toString model.setCount) ++ " times")

divc content = Html.div [padding 5] [content]

padding amt = Attr.style [("padding", (toString amt) ++ "px")]

-- UPDATE
type Action = 
    Input String
  | CookieValue (Maybe String)
  | SetOk
  | Failure String

update action model = 
  case action of
    Input str -> 
      (
        {model | input <- str},
        writeMyCookie str 
      )
    SetOk -> 
      (
        {model | setCount <- (model.setCount + 1)},
        readMyCookie cookieKey
      )
    Failure boo ->
     (
       {model | cookie <- Just ("FAILURE: " ++ boo)},
       Effects.none
     )
    CookieValue c -> 
      (
        {model | cookie <- c},
        Effects.none
      )

writeMyCookie: String -> Effects Action
writeMyCookie str = 
  writeCookie
    Failure (\_ -> SetOk) 
    { 
      key = cookieKey, 
      value = str 
    }

readMyCookie: String -> Effects Action
readMyCookie = 
  readCookie 
    (\yay -> CookieValue (Maybe.map .value yay))



