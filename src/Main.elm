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
                in div [ id "main-container" ]
                       [ div [] <| List.map viewTimer timers
                       , viewGithubLink
                       ]

viewTimer : SingleTimer -> Html Msg
viewTimer {id, model} = App.map (Modify id) (Timer.view model)

viewGithubLink : Html Msg
viewGithubLink = let url = "https://github.com/joelchelliah/multi-timer"
                 in div [ id "github-link" ]
                        [ Icon.github (Color.black) 20
                        , a [ href url ] [ text "Code is on Github" ]
                        ]
