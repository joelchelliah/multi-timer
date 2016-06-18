import Component.Timer as Timer
import Component.Animation as Animation

import Time exposing (Time, second)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Style

import AnimationFrame

import FontAwesome as Icon
import Color

main = App.program { init = init
                   , view = view
                   , update = update
                   , subscriptions = subscriptions
                   }


-- Model

type alias Model = { uid : Int
                   , timers : List TimerModel
                   }

type alias TimerModel = { model : Timer.Model
                        , anim : Style.Animation
                        }

init : (Model, Cmd Msg)
init = (Model 0 [ initTimer 0 ], Cmd.none)

initTimer : Int -> TimerModel
initTimer id =
  let model  = Timer.init id
  in TimerModel model Animation.addTimer

addTimer : Model -> Model
addTimer model =
  let newId = model.uid + 1
      timer = initTimer newId
  in { model | uid = newId
             , timers = model.timers ++ [timer]
     }

removeTimer : Int -> Model -> Model
removeTimer id model =
  let matchesId timer = timer.model.id == id
  in { model | timers = List.filter (not << matchesId) model.timers }


-- Update

type alias TimerMsg = Timer.Msg

type Msg = Modify Int TimerMsg
         | Tick
         | Add
         | Animate Float

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let return x = (x, Cmd.none)
  in case msg of
    Modify id msg -> return <| updateModel model <| modifyTimer id msg
    Tick          -> return <| updateModel model tickTimer
    Add           -> return <| addTimer model
    Animate time  -> return <| updateModel model <| tickAnimation time

updateModel : Model -> (TimerModel -> TimerModel) -> Model
updateModel model func =
  let isAlive timer = not timer.model.dead
      updatedTimers = List.filter isAlive <| List.map func model.timers
  in { model | timers = updatedTimers }

modifyTimer : Int -> TimerMsg -> TimerModel -> TimerModel
modifyTimer timerId msg ({model} as timer) =
  let model' = if timerId == model.id then Timer.update msg model else model
  in { timer | model = model' }

tickTimer : TimerModel -> TimerModel
tickTimer ({model} as timer) =
  let model' = Timer.update Timer.Tick model
  in { timer | model = model' }


tickAnimation : Float -> TimerModel -> TimerModel
tickAnimation time timer =
  { timer | anim = Style.tick time timer.anim }


-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.batch [ Time.every Time.second (\_ -> Tick)
                            , AnimationFrame.times Animate
                            ]


-- View

view : Model -> Html Msg
view model = div [ id "main-container" ]
                 [ viewTimers model
                 , viewAddTimerButton
                 , viewGithubLink
                 ]

viewTimers : Model -> Html Msg
viewTimers {timers} =
  let viewTimer {model, anim} = App.map (Modify model.id) (Timer.view model anim)
  in div [style [("overflow", "hidden")]] <| List.map viewTimer timers

viewAddTimerButton : Html Msg
viewAddTimerButton =
  let plusIcon = flip Icon.plus <| 20
      val = [ span [ class "icon" ] [ plusIcon Color.black ]
            , span [ class "icon-hover" ] [ plusIcon Color.white ]
            , span [ class "text" ] [ text "Add" ]
            ]
  in div [] [ button [ class "btn btn-add", onClick Add ] val ]

viewGithubLink : Html Msg
viewGithubLink =
  let url = "https://github.com/joelchelliah/multi-timer"
  in div [ id "github-link" ]
         [ Icon.github (Color.black) 20
         , a [ href url ] [ text "multi-timer" ]
         ]
