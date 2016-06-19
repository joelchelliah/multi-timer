import Component.Timer as Timer

import Time exposing (Time, second)
import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
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

type alias TimerModel = Timer.Model

init : (Model, Cmd Msg)
init = (Model 0 [ Timer.init 0 ], Cmd.none)

addTimer : Model -> Model
addTimer model =
  let newId = model.uid + 1
  in { model | uid = newId
             , timers = model.timers ++ [Timer.init newId]
     }


-- Update

type alias TimerMsg = Timer.Msg

type Msg = Modify Int TimerMsg
         | Tick
         | Add
         | Animate Float

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let return x = (x, Cmd.none)
      updateTimers func model = { model | timers = List.map func model.timers }
  in case msg of
    Modify id msg -> return <| modifyTimer model id msg
    Add           -> return <| addTimer model
    Tick          -> return <| updateTimers Timer.tick model
    Animate time  -> return <| updateTimers (Timer.tickAnimation time) model

modifyTimer : Model -> Int -> TimerMsg -> Model
modifyTimer model id msg =
  let handleState timer acc = case Timer.handleState timer of
        Just (timer') -> timer' :: acc
        Nothing       -> acc
      modify timer = if id == timer.id
                     then Timer.update msg timer
                     else timer
  in { model | timers = model.timers
                     |> List.map modify
                     |> List.foldr handleState []
     }

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.batch [ Time.every Time.second (\_ -> Tick)
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
  let viewTimer timer = App.map (Modify timer.id) (Timer.view timer)
  in div [] (List.map viewTimer timers)

viewAddTimerButton : Html Msg
viewAddTimerButton =
  let icon cls col = span [ class cls ] [ Icon.plus col 20 ]
      val = [ icon "icon" Color.black
            , icon "icon-hover" Color.white
            , span [ class "text" ] [ text "Add Timer" ]
            ]
  in button [ class "btn btn-add", onClick Add ] val

viewGithubLink : Html Msg
viewGithubLink =
  let icon = Icon.github (Color.black) 20
      url  = "https://github.com/joelchelliah/multi-timer"
      link = a [ href url ] [ text "multi-timer" ]
  in div [ id "github-link" ] [ icon, link ]
