module Component.Timer exposing ( Model, Msg
                                , init, update, view
                                , tick, tickAnimation, handleState )

import Component.Duration as Duration
import Component.Animation as Animation

import Html.App as App
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Style
import Color
import FontAwesome as Icon


-- Model

type alias Model = { duration : Duration.Model
                   , state : State
                   , animation: Style.Animation
                   , id : Int
                   }

type State = Playing
           | Paused
           | Stopped
           | Dying
           | Dead

init : Int -> Model
init = initWithDuration Duration.init

initWithDuration : Duration.Model -> Int -> Model
initWithDuration duration = Model duration Paused Animation.addTimer


-- Update

type Msg = Modify Duration.Msg
         | Start
         | Pause
         | Stop
         | Kill

update : Msg -> Model -> Model
update msg ({duration} as model) =
  case msg of
    Modify msg -> { model | duration = Duration.update msg duration, state = Paused }

    Start -> if Duration.isZero duration then model else { model | state = Playing }
    Pause -> { model | state = Paused }
    Stop  -> { model | state = Stopped, duration = Duration.init }
    Kill  -> { model | state = Dying }

tick : Model -> Model
tick ({duration, state} as model) =
  if Duration.isZero duration
  then { model | state = Stopped }
  else if state == Playing then tickDuration model else model

tickDuration : Model -> Model
tickDuration model =
  { model | duration = Duration.tick model.duration }

tickAnimation : Float -> Model -> Model
tickAnimation time model =
  { model | animation = Style.tick time model.animation }

handleState : Model -> Maybe Model
handleState model = case model.state of
  Dead  -> Nothing
  Dying -> Just (die model)
  _     -> Just model

die : Model -> Model
die model = { model | state = Dead, animation = Animation.removeTimer }


-- View

view : Model -> Html Msg
view model =
  let animationStyle = Style.render model.animation
  in span [ style animationStyle, class ("timer") ]
          [ viewDuration model
          , viewPlayerButtonGroup model
          ]


type alias IconFunc = (Color.Color -> Int -> Html Msg)

viewDuration : Model -> Html Msg
viewDuration {duration, state} =
  let enabled = state == Playing
  in Duration.view duration enabled
  |> App.map Modify

viewPlayerButtonGroup : Model -> Html Msg
viewPlayerButtonGroup { state } =
  let playOrPause = if state == Playing
                    then viewPlayerButton Pause Icon.pause
                    else viewPlayerButton Start Icon.play
  in div [] [ playOrPause
            , viewPlayerButton Stop Icon.stop
            , viewPlayerButton Kill Icon.remove
            ]

viewPlayerButton : Msg -> IconFunc -> Html Msg
viewPlayerButton msg icon =
  let iconPair = [ span [ class "icon" ] [ icon Color.black 15 ]
                 , span [ class "icon-hover" ] [ icon Color.white 15 ]
                 ]
  in button [ class "btn btn-player", onClick msg ] iconPair
