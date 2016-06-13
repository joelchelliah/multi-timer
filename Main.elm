import Timer
import Time exposing (Time, second)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)

main = App.program { init = init
                   , view = view
                   , update = update
                   , subscriptions = subscriptions
                   }


-- Model

type alias Model = { timers : List SingleTimer }

type alias SingleTimer = { id : Int
                         , model: Timer.Model
                         }

init : (Model, Cmd Msg)
init = let timers = Model [ SingleTimer 0 (Timer.init 10)
                          , SingleTimer 1 (Timer.init 25)
                          ]
       in (timers, Cmd.none)


-- Update

type Msg = Modify Int Timer.Msg
         | Tick Time

update : Msg -> Model -> (Model, Cmd Msg)
update msg ({timers} as model) = case msg of
  Modify id msg ->
    ({ model | timers = List.map (updateTimer id msg) timers}, Cmd.none)
  Tick _ ->
    ({ model | timers = List.map tickTimer timers}, Cmd.none)

updateTimer : Int -> Timer.Msg -> SingleTimer -> SingleTimer
updateTimer timerId msg {id, model} = let model' = if timerId == id
                                                   then Timer.update msg model
                                                   else model
                                      in SingleTimer id model'

tickTimer : SingleTimer -> SingleTimer
tickTimer {id, model} = SingleTimer id <| Timer.tick model

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model = Time.every Time.second Tick


-- View

view : Model -> Html Msg
view {timers} = let running = List.any (\t -> t.model.duration > 0) timers
                in div [ mainStyle running ]
                       [ img (clockAttributes running) []
                       , div [] <| List.map viewTimer timers
                       ]

viewTimer : SingleTimer -> Html Msg
viewTimer {id, model} = App.map (Modify id) (Timer.view model)


mainStyle : Bool -> Attribute msg
mainStyle running = style [ ("font-family", "'Times New Roman', Times, serif")
                          , ("text-align", "center")
                          , ("font-size", "20px")
                          , ("display", "block")
                          , ("height", "100%")
                          , ("background-color", if running
                                                 then "#EDF3F7"
                                                 else "#DFD"
                                                 )
                          ]

clockAttributes : Bool -> List (Attribute msg)
clockAttributes running =
  let inProgress = "http://thininc.com/wp-content/uploads/2013/04/alarm-clock-p.com-162667796.jpg"
      done       = "http://ajax.raffertyaluminum.com/pics/completed_stamp.gif"
  in [ src <| if running
              then inProgress
              else done
     , height 200
     , style [ ("padding", "2em") ]
     ]
