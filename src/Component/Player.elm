module Component.Player exposing ( Model, Msg, init, update, view
                                 , tick
                                 )

import Component.Duration  as Duration

import Html.App as App
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import FontAwesome as Icon
import Color

-- Model

type alias Model = { state: State
                   , duration: Duration.Model
                   }

type State = Playing
           | Paused
           | Stopped
           | Completed

init : Model
init = Model Stopped Duration.init


-- Update

type Msg = Play
         | Pause
         | Stop
         | Modify Duration.Msg

update : Msg -> Model -> Model
update msg model =
  let isPlaying = model.state == Playing
      isZero    = Duration.isZero model.duration
  in case msg of
    Play       -> { model | state = if isZero then Stopped else Playing }
    Pause      -> { model | state = Paused  }
    Stop       -> { model | state = Stopped, duration = Duration.init }
    Modify msg -> if isPlaying
                  then model
                  else { model | duration = Duration.update msg model.duration }

tick : Model -> Model
tick ({state, duration} as model) =
  if state == Playing && not (Duration.isZero duration)
  then tickDuration model
  else model

tickDuration : Model -> Model
tickDuration model =
  let newDuration = Duration.tick model.duration
  in if Duration.isZero newDuration
     then { model | duration = newDuration, state = Completed }
     else { model | duration = newDuration }

-- View

view : Model -> Html Msg
view model =
  let playOrPause = if model.state == Playing
                    then viewButton Pause Icon.pause "Pause"
                    else viewButton Play Icon.play "Play"
  in div [] [ viewDuration model
            , playOrPause
            , viewButton Stop Icon.stop "Stop"
            ]

viewDuration : Model -> Html Msg
viewDuration {state, duration} =
  let enabled   = state == Playing
      completed = state == Completed
  in Duration.view duration enabled completed |> App.map Modify

type alias IconFunc = (Color.Color -> Int -> Html Msg)

viewButton : Msg -> IconFunc -> String -> Html Msg
viewButton msg iconFunc txt =
  let icon cls col = span [ class cls ] [ iconFunc col 15 ]
      val          = [ icon "icon" Color.black
                     , icon "icon-hover" Color.white
                     , span [ class "text" ] [ text txt ]
                     ]
  in button [ class "btn btn-player", onClick msg ] val
