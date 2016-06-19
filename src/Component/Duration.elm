module Component.Duration exposing ( Model, Msg, init, update, tick, view
                                   , isZero )

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import String exposing (toInt)
import FontAwesome as Icon
import Color


type alias Model = { hours: Int
                   , minutes: Int
                   , seconds: Int
                   }

init : Model
init = Model 0 0 0

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

view : Model -> Bool -> Html Msg
view model running =
  let inputGroup = viewInputGroup model running
      incGroup   = viewButtonGroup Icon.plus  [ IncHours, IncMins, IncSecs ]
      decGroup   = viewButtonGroup Icon.minus [ DecHours, DecMins, DecSecs ]
  in inputGroup

type alias IconFunc = (Color.Color -> Int -> Html Msg)

viewButtonGroup : IconFunc -> List Msg -> Html Msg
viewButtonGroup iconFunc messages =
  div [] <| List.map (viewButton iconFunc) messages

viewButton : IconFunc -> Msg -> Html Msg
viewButton iconFunc msg =
  let icon = flip iconFunc <| 10
      val  = [ span [ class "icon" ] [ icon Color.black ]
             , span [ class "icon-hover" ] [ icon Color.white ]
             ]
  in button [ class "btn btn-adjust", onClick msg ] val

viewInputGroup : Model -> Bool -> Html Msg
viewInputGroup model running =
  let inputs = [ viewInput model.hours running   SetHours
               , viewInput model.minutes running SetMins
               , viewInput model.seconds running SetSecs
               ]
  in List.intersperse (text ":") inputs
  |> div [ class "input-group" ]

viewInput : Int -> Bool -> (String -> Msg) -> Html Msg
viewInput val dis msg =
  input [ value <| toString val
        , disabled dis
        , onInput msg
        ] []
