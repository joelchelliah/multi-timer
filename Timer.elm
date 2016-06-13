module Timer exposing (Model, Msg, init, update, tick, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)

import String exposing (toInt)

-- Model

type alias Model = { duration : Int
                   , running : Bool
                   }

init : Int -> Model
init duration = Model duration False


-- Update

type Msg = Set String
         | Start
         | Pause
         | Stop

update : Msg -> Model -> Model
update msg model =
  let durationToInt = toInt >> Result.toMaybe >> Maybe.withDefault 0
  in case msg of
    Set d -> Model (durationToInt d) False
    Start -> if model.duration > 0
              then { model | running = True }
              else model
    Pause -> { model | running = False }
    Stop  -> Model 0 False

tick : Model -> Model
tick ({duration, running} as model) = case (duration, running) of
  (0, _)    -> Model 0 False
  (d, True) -> Model (d - 1) True
  _         -> model

-- View

view : Model -> Html Msg
view model = div []
                 -- [ div [] [ text <| toString model.duration ]
                 [ div [] [ button [ durationStyle, onClick (Set "7200") ] [ text "2h" ]
                          , button [ durationStyle, onClick (Set "5400") ] [ text "1h 30min" ]
                          , button [ durationStyle, onClick (Set "2700") ] [ text "45min" ]
                          , button [ durationStyle, onClick (Set "10") ] [ text "10s" ]
                          ]
                 , div [] [ input [ placeholder "Set duration"
                          , value <| toString model.duration
                          , disabled model.running
                          , onInput Set
                          , inputStyle model
                          ] [ ] ]
                 , div [] [ button [ buttonStyle, onClick Start ] [ text "Start" ]
                          , button [ buttonStyle, onClick Pause ] [ text "Pause" ]
                          , button [ buttonStyle, onClick Stop ] [ text "Stop" ]
                          ]
                 ]

durationStyle : Attribute msg
durationStyle = style [ ("text-align", "center")
                      , ("margin", "0.2em 0.4em")
                      , ("font-size", "1em")
                      , ("display", "inline-block")
                      , ("-webkit-border-radius", "0.4em")
                      , ("background-color", "#DDF")
                      ]

inputStyle : Model -> Attribute msg
inputStyle {duration, running} =
  let doneColor = "#6FA"
      runningColor = "#DEF"
      stoppedColor = "#DDF"
  in style [ ("text-align", "center")
           , ("margin", "0.2em auto")
           , ("font-size", "50px")
           , ("display", "block")
           , ("border", "none")
           , ("border-bottom", "solid 1px #ddd")
           , ("-webkit-border-radius", "0.4em")
           , ("background-color", if duration == 0
                                  then doneColor
                                  else if running
                                       then runningColor
                                       else stoppedColor)
           ]

buttonStyle : Attribute msg
buttonStyle = style [ ("text-align", "center")
                    , ("margin", "0.2em 0.4em")
                    , ("font-size", "1.5em")
                    , ("display", "inline-block")
                    , ("-webkit-border-radius", "0.4em")
                    ]
