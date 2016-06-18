import Component.Timer as Timer
import Time exposing (Time, second)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import FontAwesome as Icon
import Color

main = App.program { init = init
                   , view = view
                   , update = update
                   , subscriptions = subscriptions
                   }


-- Model

type alias TimerModel = Timer.Model
type alias TimerMsg = Timer.Msg

type alias Model = { uid : Int
                   , timers : List TimerModel
                   }

init : (Model, Cmd Msg)
init = (Model 0 [ Timer.init 0 ], Cmd.none)

addTimer : Model -> Model
addTimer model = let newId = model.uid + 1
                     timer = Timer.init newId
                 in { model | uid = newId
                            , timers = model.timers ++ [timer]
                    }

removeTimer : Int -> Model -> Model
removeTimer id model =
  let matchesId timer = timer.id == id
  in { model | timers = List.filter (not << matchesId) model.timers }


-- Update

type Msg = Modify Int TimerMsg
         | Tick
         | Add

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let return x = (x, Cmd.none)
  in case msg of
    Modify id msg -> return <| updateModel model <| modifyTimer id msg
    Tick          -> return <| updateModel model tickTimer
    Add           -> return <| addTimer model

updateModel : Model -> (TimerModel -> TimerModel) -> Model
updateModel model func =
  let isAlive timer = not timer.dead
      updatedTimers = List.filter isAlive <| List.map func model.timers
  in { model | timers = updatedTimers }

modifyTimer : Int -> TimerMsg -> TimerModel -> TimerModel
modifyTimer timerId msg timer =
  if timerId == timer.id then Timer.update msg timer else timer

tickTimer : TimerModel -> TimerModel
tickTimer = Timer.update Timer.Tick


-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model = Time.every Time.second (\_ -> Tick)


-- View

view : Model -> Html Msg
view model = div [ id "main-container" ]
                 [ viewTimers model
                 , viewAddTimerButton
                 , viewGithubLink
                 ]

viewTimers : Model -> Html Msg
viewTimers {timers} =
  let viewTimer timer = App.map (Modify timer.id) (Timer.view timer)
  in div [] <| List.map viewTimer timers

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
