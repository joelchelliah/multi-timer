module Component.Duration exposing ( Model, Msg
                                   , init, update, view, tick, isZero )

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import String exposing (toInt)
import FontAwesome as Icon
import Color


-- Model

type alias Model = { hours: Int
                   , minutes: Int
                   , seconds: Int
                   }

init : Model
init = Model 0 0 0


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

update : Msg -> Model -> Model
update msg ({hours, minutes, seconds}  as model) =
  let trunc i = if i < 0 then 0 else if i > 999 then 999 else i
      toInt'  = toInt >> Result.toMaybe >> Maybe.map trunc >> Maybe.withDefault 0
  in case msg of
    SetHours h -> { model | hours   = toInt' h }
    SetMins  m -> { model | minutes = toInt' m }
    SetSecs  s -> { model | seconds = toInt' s }
    IncHours   -> { model | hours   = trunc <| hours   + 1 }
    IncMins    -> { model | minutes = trunc <| minutes + 1 }
    IncSecs    -> { model | seconds = trunc <| seconds + 1 }
    DecHours   -> { model | hours   = trunc <| hours   - 1 }
    DecMins    -> { model | minutes = trunc <| minutes - 1 }
    DecSecs    -> { model | seconds = trunc <| seconds - 1 }

tick : Model -> Model
tick ({hours, minutes, seconds}  as model) =
  case (hours, minutes, seconds) of
    (0, 0, 0) -> model
    (h, 0, 0) -> { model | hours   = h - 1, minutes = 59, seconds = 59 }
    (_, m, 0) -> { model | minutes = m - 1, seconds = 59 }
    (_, _, s) -> { model | seconds = s - 1 }

isZero : Model -> Bool
isZero {hours, minutes, seconds} = 3600 * hours + 60 * minutes + seconds == 0


-- View

view : Model -> Bool -> Html Msg
view model enabled =
  div [] [ viewButtonGroup Icon.plus  [ IncHours, IncMins, IncSecs ]
         , viewInputGroup model enabled
         , viewButtonGroup Icon.minus [ DecHours, DecMins, DecSecs ]
         ]

type alias IconFunc = (Color.Color -> Int -> Html Msg)

viewButtonGroup : IconFunc -> List Msg -> Html Msg
viewButtonGroup iconFunc messages =
  let buttonFunc = viewButton iconFunc
  in messages
  |> List.map buttonFunc
  |> div [ class "btn-group" ]

viewButton : IconFunc -> Msg -> Html Msg
viewButton iconFunc msg =
  let icon klass col = span [ class klass ] [ iconFunc col 10 ]
      iconGroup      = [ icon "icon" Color.black
                       , icon "icon-hover" Color.white
                       ]
  in button [ class "btn btn-adjust", onClick msg ] iconGroup

viewInputGroup : Model -> Bool -> Html Msg
viewInputGroup model enabled =
  let inputs = [ viewInput model.hours   enabled SetHours
               , viewInput model.minutes enabled SetMins
               , viewInput model.seconds enabled SetSecs
               ]
  in List.intersperse (text ":") inputs
  |> div [ class "input-group" ]

viewInput : Int -> Bool -> (String -> Msg) -> Html Msg
viewInput num disable msg =
  let val = toString num
  in input [ value val
           , disabled disable
           , onInput msg
           ] []
