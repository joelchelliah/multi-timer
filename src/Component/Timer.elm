module Component.Timer exposing ( Model, Msg, init, update, view
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
                   , running : Bool
                   , state : State
                   , animation: Style.Animation
                   , id : Int
                   }

type State = Alive | Dying | Dead

init : Int -> Model
init = initWithDuration Duration.init

initWithDuration : Duration.Model -> Int -> Model
initWithDuration duration = Model duration False Alive Animation.addTimer


-- Update

type Msg = Modify Duration.Msg
         | Start
         | Pause
         | Stop
         | Kill

update : Msg -> Model -> Model
update msg ({duration, running, id, animation} as model) =
  case msg of
    Modify msg -> { model | duration = Duration.update msg duration, running = False }

    Start -> if Duration.isZero duration then model else { model | running = True }
    Pause -> { model | running = False }
    Stop  -> { model | running = False, duration = Duration.init }
    Kill  -> { model | state = Dying }

tick : Model -> Model
tick ({duration, running} as model) =
  if Duration.isZero duration
  then { model | running = False }
  else if running
       then { model | duration = Duration.tick duration }
       else model

tickAnimation : Float -> Model -> Model
tickAnimation time model = { model | animation = Style.tick time model.animation }

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
          [ --viewTimerButtonGroup Icon.plus Duration.messagesInc
          --,
          viewDuration model
          --, viewTimerButtonGroup Icon.minus Duration.messagesDec
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

viewDuration : Model -> Html Msg
viewDuration {duration, running} =
  let durationView = Duration.view duration running
  in App.map Modify durationView

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
