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
init = let timers = Model [ SingleTimer 0 (Timer.init)
                          , SingleTimer 1 (Timer.init)
                          ]
       in (timers, Cmd.none)


-- Update

type Msg = Modify Int Timer.Msg
         | Tick

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let updateModel f = { model | timers = List.map f model.timers}
  in case msg of
    Modify id msg -> (updateModel (modifyTimer id msg), Cmd.none)
    Tick          -> (updateModel tickTimer, Cmd.none)

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
view {timers} = div [ id "main-container" ]
                    [ div [] <| List.map viewTimer timers
                    , viewGithubLink
                    ]

viewTimer : SingleTimer -> Html Msg
viewTimer {id, model} = App.map (Modify id) (Timer.view model)

viewGithubLink : Html Msg
viewGithubLink = let url = "https://github.com/joelchelliah/multi-timer"
                 in div [ id "github-link" ]
                        [ Icon.github (Color.black) 20
                        , a [ href url ] [ text "multi-timer" ]
                        ]
