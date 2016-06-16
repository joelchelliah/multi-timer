module Component.Timer exposing (Model, Msg(Tick), init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import FontAwesome
import String exposing (toInt)
import Color

-- Model

type alias Model = { duration : Duration
                   , running : Bool
                   }

type alias Duration = { hours: Int
                      , minutes: Int
                      , seconds: Int
                      }

init : Model
init = Model (Duration 0 0 0) False

initWithDuration : Duration -> Model
initWithDuration d = Model d False

durationInSeconds : Duration -> Int
durationInSeconds {hours, minutes, seconds} =
  3600 * hours + 60 * minutes + seconds

-- Update

type Msg = SetHours String
         | SetMins  String
         | SetSecs  String
         | IncHours
         | IncMins
         | IncSecs
         | DecHours
         | DecMins
         | DecSecs
         | Start
         | Pause
         | Stop
         | Tick

update : Msg -> Model -> Model
update msg ({duration, running} as model) =
  let trunc d = if d < 0 then 0 else if d > 999 then 999 else d
      inputToInt = toInt >> Result.toMaybe >> Maybe.map trunc >> Maybe.withDefault 0
      setDuration d = Model d False
      finished = durationInSeconds duration == 0
  in case msg of
    SetHours h -> setDuration { duration | hours = inputToInt h }
    SetMins  m -> setDuration { duration | minutes = inputToInt m }
    SetSecs  s -> setDuration { duration | seconds = inputToInt s }

    IncHours -> setDuration { duration | hours   = trunc <| duration.hours   + 1 }
    IncMins  -> setDuration { duration | minutes = trunc <| duration.minutes + 1 }
    IncSecs  -> setDuration { duration | seconds = trunc <| duration.seconds + 1 }
    DecHours -> setDuration { duration | hours   = trunc <| duration.hours   - 1 }
    DecMins  -> setDuration { duration | minutes = trunc <| duration.minutes - 1 }
    DecSecs  -> setDuration { duration | seconds = trunc <| duration.seconds - 1 }

    Start -> if finished then model else { model | running = True }
    Pause -> { model | running = False }
    Stop  -> init
    Tick -> if finished
            then { model | running = False }
            else if running
                 then { model | duration = updateDuration duration }
                 else model

updateDuration : Duration -> Duration
updateDuration d =
  if d.seconds > 0
  then { d | seconds = d.seconds - 1 }
  else if d.minutes > 0
       then { d | minutes = d.minutes - 1
                , seconds = 59
            }
       else if d.hours > 0
            then { d | hours = d.hours - 1
                     , minutes = 59
                     , seconds = 59
                 }
            else d

tick : Model -> Model
tick ({duration, running} as model) =
  if durationInSeconds duration == 0
  then { model | running = False }
  else if running
       then { model | duration = updateDuration duration }
       else model

-- View

view : Model -> Html Msg
view model = let iconColor = Color.rgb 190 190 240
                 playIcon  = FontAwesome.play iconColor 20
                 pauseIcon = FontAwesome.pause iconColor 20
                 stopIcon  = FontAwesome.stop iconColor 20
             in span [ class "timer" ]
                     [ viewIncButtonGroup
                     , viewInputGroup model
                     , viewDecButtonGroup
                     , div []
                           [ button [ style buttonStyle, onClick Start ] [ playIcon ]
                           , button [ style buttonStyle, onClick Pause ] [ pauseIcon ]
                           , button [ style buttonStyle, onClick Stop ] [ stopIcon ]
                           ]
                     ]

viewIncButtonGroup : Html Msg
viewIncButtonGroup = div []
                         [ viewAdjustButton IncHours "+"
                         , viewAdjustButton IncMins "+"
                         , viewAdjustButton IncSecs "+"
                         ]

viewDecButtonGroup : Html Msg
viewDecButtonGroup = div []
                         [ viewAdjustButton DecHours "-"
                         , viewAdjustButton DecMins "-"
                         , viewAdjustButton DecSecs "-"
                         ]

viewAdjustButton : Msg -> String -> Html Msg
viewAdjustButton msg txt = button [ class "btn adjust-btn", onClick msg ] [ text txt ]

viewInputGroup : Model -> Html Msg
viewInputGroup {duration, running} =
  let seperator = text ":"
  in div [ class "input-group" ]
         [ viewInput duration.hours running SetHours
         , seperator
         , viewInput duration.minutes running SetMins
         , seperator
         , viewInput duration.seconds running SetSecs
         ]

viewInput : Int -> Bool -> (String -> Msg) -> Html Msg
viewInput val dis msg = input [ value <| toString val
                              , disabled dis
                              , onInput msg
                              ] []
-- Style

type alias Style = List ( String, String )

buttonStyle : Style
buttonStyle = [ ("text-align", "center")
              , ("margin", "0.2em 0.4em")
              , ("padding", "0.25em 0.9em 0.1em")
              , ("font-size", "1em")
              , ("display", "inline-block")
              , ("background-color", "#114")
              , ("border-color", "#55A")
              , ("-webkit-border-radius", "0.4em")
              ]

durationStyle : Style
durationStyle = [ ("text-align", "center")
                , ("margin", "0.2em 0.4em")
                , ("font-size", "0.6em")
                , ("display", "inline-block")
                , ("color", "#BBE")
                , ("background-color", "#114")
                , ("border-color", "#55A")
                , ("-webkit-border-radius", "0.4em")
                ]
