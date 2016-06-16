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

type alias Model = { uid : Int
                   , timers : List SingleTimer
                   }

type alias SingleTimer = { id : Int
                         , model: Timer.Model
                         }

init : (Model, Cmd Msg)
init = (Model 0 [ initTimer 0 ], Cmd.none)

initTimer : Int -> SingleTimer
initTimer id = SingleTimer id (Timer.init)

addTimer : Model -> Model
addTimer model = let newId = model.uid + 1
                     timer = initTimer newId
                 in { model | uid = newId
                            , timers = model.timers ++ [timer]
                    }

removeTimer : Int -> Model -> Model
removeTimer id model =
  let matchesId timer = timer.id == id
  in { model | timers = List.filter (not << matchesId) model.timers }


-- Update

type Msg = Modify Int Timer.Msg
         | Tick
         | Add
         | Remove Int

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let updateModel f = { model | timers = List.map f model.timers}
      return x      = (x, Cmd.none)
  in case msg of
    Modify id msg -> return <| updateModel <| modifyTimer id msg
    Tick          -> return <| updateModel tickTimer
    Add           -> return <| addTimer model
    Remove id     -> return <| removeTimer id model

modifyTimer : Int -> Timer.Msg -> SingleTimer -> SingleTimer
modifyTimer timerId msg {id, model} =
  let model' = if timerId == id then Timer.update msg model else model
  in SingleTimer id model'

tickTimer : SingleTimer -> SingleTimer
tickTimer {id, model} =
  let model' = Timer.update Timer.Tick model
  in SingleTimer id model'


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
  let viewTimer {id, model} = App.map (Modify id) (Timer.view model)
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
