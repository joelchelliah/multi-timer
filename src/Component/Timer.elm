module Component.Timer exposing (Model, Msg(Tick), init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import FontAwesome as Icon
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
view model = span [ class "timer" ]
                  [ viewTimerButtonGroup Icon.plus
                  , viewInputGroup model
                  , viewTimerButtonGroup Icon.minus
                  , viewPlayerButtonGroup
                  ]

viewTimerButtonGroup : (Color.Color -> Int -> Html Msg) -> Html Msg
viewTimerButtonGroup iconFunc =
  let icon = flip iconFunc <| 10
      val  = [ span [ class "icon" ] [ icon Color.black ]
             , span [ class "icon-hover" ] [ icon Color.white ]
             ]
      viewButton msg = button [ class "btn btn-adjust", onClick msg ] val
  in div [] [ viewButton IncHours
            , viewButton IncMins
            , viewButton IncSecs
            ]

viewPlayerButtonGroup : Html Msg
viewPlayerButtonGroup =
  let pair icon = [ span [ class "icon" ] [ icon Color.black 15 ]
                  , span [ class "icon-hover" ] [ icon Color.white 15 ]
                  ]
      viewButton msg icon = button [ class "btn btn-player", onClick msg ] icon
  in div [] [ viewButton Start <| pair Icon.play
            , viewButton Pause <| pair Icon.pause
            , viewButton Stop  <| pair Icon.stop
            ]

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
