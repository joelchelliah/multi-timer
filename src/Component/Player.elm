module Component.Player exposing ( Model, Msg, init, update, view
                                 , isPlaying
                                 )

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import FontAwesome as Icon
import Color

-- Model

type alias Model = { state: State }

type State = Playing
           | Paused
           | Stopped

init : Model
init = Model Stopped


-- Update

type Msg = Play
         | Pause
         | Stop

update : Msg -> Model -> Model
update msg ({state} as model) =
  case msg of
    Play  -> { model | state = Playing }
    Pause -> { model | state = Paused  }
    Stop  -> { model | state = Stopped }

isPlaying : Model -> Bool
isPlaying {state} = state == Playing


-- View

view : Model -> Html Msg
view {state} =
  let playOrPause = if state == Playing
                    then viewButton Pause Icon.pause "Pause"
                    else viewButton Play Icon.play "Play"
  in div [] [ playOrPause
            , viewButton Stop Icon.stop "Stop"
            ]

type alias IconFunc = (Color.Color -> Int -> Html Msg)

viewButton : Msg -> IconFunc -> String -> Html Msg
viewButton msg iconFunc txt =
  let icon cls col = span [ class cls ] [ iconFunc col 20 ]
      val          = [ icon "icon" Color.black
                     , icon "icon-hover" Color.white
                     , span [ class "text" ] [ text txt ]
                     ]
  in button [ class "btn btn-player", onClick msg ] val
