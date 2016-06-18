module Component.Timer exposing ( Model, Msg, State(Alive, Dying, Dead)
                                , init, update, view
                                , tick, tickAnimation, handleState )

import Component.Animation as Animation

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Style
import Color
import FontAwesome as Icon
import String exposing (toInt)


-- Model

type alias Model = { duration : Duration
                   , running : Bool
                   , state : State
                   , animation: Style.Animation
                   , id : Int
                   }

type alias Duration = { hours: Int
                      , minutes: Int
                      , seconds: Int
                      }

type State = Alive
           | Dying
           | Dead

init : Int -> Model
init = initWithDuration (Duration 0 0 0)

initWithDuration : Duration -> Int -> Model
initWithDuration duration = Model duration False Alive Animation.addTimer

isTimerZero : Duration -> Bool
isTimerZero {hours, minutes, seconds} =
  3600 * hours + 60 * minutes + seconds == 0

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
         | Kill

update : Msg -> Model -> Model
update msg ({duration, running, id, animation} as model) =
  let trunc d = if d < 0 then 0 else if d > 999 then 999 else d
      inputToInt = toInt >> Result.toMaybe >> Maybe.map trunc >> Maybe.withDefault 0
      setDuration d = Model d False Alive animation id
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

    Start -> if isTimerZero duration then model else { model | running = True }
    Pause -> { model | running = False }
    Stop  -> { model | running = False, duration = Duration 0 0 0 }
    Kill  -> { model | state = Dying }

tick : Model -> Model
tick ({duration, running} as model) =
  if isTimerZero duration
  then { model | running = False }
  else if running
       then { model | duration = tickDuration duration }
       else model

tickAnimation : Float -> Model -> Model
tickAnimation time model = { model | animation = Style.tick time model.animation }

tickDuration : Duration -> Duration
tickDuration d =
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

handleState : Model -> Maybe Model
handleState model = case model.state of
  Alive -> Just model
  Dying -> Just (die model)
  Dead  -> Nothing


die : Model -> Model
die model = { model | state = Dead, animation = Animation.removeTimer }


-- View

view : Model -> Html Msg
view model =
  let animationStyle = [("position", "relative")] ++ Style.render model.animation
  in span [ style animationStyle, class ("timer " ++ toString model.state) ]
          [ viewTimerButtonGroup Icon.plus [IncHours, IncMins, IncSecs]
          , viewInputGroup model
          , viewTimerButtonGroup Icon.minus [DecHours, DecMins, DecSecs]
          , viewPlayerButtonGroup model
          ]


type alias IconFunc = (Color.Color -> Int -> Html Msg)

viewTimerButtonGroup : IconFunc -> List Msg -> Html Msg
viewTimerButtonGroup iconFunc messages =
  let icon = flip iconFunc <| 10
      val  = [ span [ class "icon" ] [ icon Color.black ]
             , span [ class "icon-hover" ] [ icon Color.white ]
             ]
      viewButton msg = button [ class "btn btn-adjust", onClick msg ] val
  in div [] <| List.map viewButton messages

viewPlayerButtonGroup : Model -> Html Msg
viewPlayerButtonGroup { running } =
  let playOrPause = if running
                    then viewPlayerButton Pause <| Icon.pause
                    else viewPlayerButton Start <| Icon.play
  in div [] [ playOrPause
            , viewPlayerButton Stop <| Icon.stop
            , viewPlayerButton Kill <| Icon.remove
            ]

viewPlayerButton : Msg -> IconFunc -> Html Msg
viewPlayerButton msg icon =
  let iconPair = [ span [ class "icon" ] [ icon Color.black 15 ]
                 , span [ class "icon-hover" ] [ icon Color.white 15 ]
                 ]
  in button [ class "btn btn-player", onClick msg ] iconPair

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
