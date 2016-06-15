module Component.Timer exposing (Model, Msg, init, update, tick, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import FontAwesome
import String exposing (toInt)
import Color

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
view model = let iconColor = Color.rgb 190 190 240
                 playIcon  = FontAwesome.play iconColor 20
                 pauseIcon = FontAwesome.pause iconColor 20
                 stopIcon  = FontAwesome.stop iconColor 20
             in span [ style mainStyle ]
                     [ div [ style buttonGroupStyle ]
                           [ button [ style durationStyle, onClick (Set "7200") ] [ text "2h" ]
                           , button [ style durationStyle, onClick (Set "5400") ] [ text "1h 30min" ]
                           , button [ style durationStyle, onClick (Set "2700") ] [ text "45min" ]
                           , button [ style durationStyle, onClick (Set "10") ] [ text "10s" ]
                           ]
                     , div [ style inputGroupStyle ]
                           [ input [ placeholder "Set duration"
                                   , value <| toString model.duration
                                   , disabled model.running
                                   , onInput Set
                                   , style (inputStyle model)
                                   ] [] ]
                     , div [ style buttonGroupStyle ]
                           [ button [ style buttonStyle, onClick Start ] [ playIcon ]
                           , button [ style buttonStyle, onClick Pause ] [ pauseIcon ]
                           , button [ style buttonStyle, onClick Stop ] [ stopIcon ]
                           ]
                     ]


-- Style

type alias Style = List ( String, String )

mainStyle : Style
mainStyle = [ ("display", "block")
            , ("padding-bottom", "0.8em")
            , ("margin-top", "0.8em")
            , ("border-bottom", "solid 1px rgb(190, 190, 240)")
            ]

buttonGroupStyle : Style
buttonGroupStyle = [ ("display", "block") ]

inputGroupStyle : Style
inputGroupStyle = [ ("display", "block")
                    , ("margin-left", "0.2em")
                    ]


buttonStyle : Style
buttonStyle = [ ("text-align", "center")
              , ("margin", "0.2em 0.4em")
              , ("padding", "0.25em 0.9em 0.1em")
              , ("font-size", "1em")
              , ("display", "inline-block")
              , ("background-color", "#114")
              , ("border-color", "#55A")
              , ("-webkit-border-radius", "0.4em")
              ]

durationStyle : Style
durationStyle = [ ("text-align", "center")
                , ("margin", "0.2em 0.4em")
                , ("font-size", "0.6em")
                , ("display", "inline-block")
                , ("color", "#BBE")
                , ("background-color", "#114")
                , ("border-color", "#55A")
                , ("-webkit-border-radius", "0.4em")
                ]

inputStyle : Model -> Style
inputStyle {duration, running} =
  let doneColor = "#6FA"
      runningColor = "#DEF"
      stoppedColor = "#DDF"
      bgColor = if duration == 0
                then doneColor
                else if running then runningColor else stoppedColor
  in [ ("text-align", "center")
     , ("margin", "0.2em auto")
     , ("font-size", "1em")
     , ("display", "inline-block")
     , ("border", "none")
     , ("border-bottom", "solid 1px #ddd")
     , ("-webkit-border-radius", "0.4em")
     , ("background-color", bgColor)
     ]
