module Component.Timer exposing ( Model, Msg, init, update, view
                                , tick, tickAnimation, handleState )

import Component.Player    as Player
import Component.Animation as Animation

import Html.App as App
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Style
import Color
import FontAwesome as Icon


-- Model

type alias Model = { player : Player.Model
                   , state : State
                   , animation: Style.Animation
                   , id : Int
                   }

type State = Alive
           | Dying
           | Dead

init : Int -> Model
init = Model Player.init Alive Animation.addTimer


-- Update

type Msg = Kill
         | Modify Player.Msg

update : Msg -> Model -> Model
update msg model =
  case msg of
    Kill       -> { model | state  = Dying }
    Modify msg -> { model | player = Player.update msg model.player }

tick : Model -> Model
tick model = { model | player = Player.tick model.player }

tickAnimation : Float -> Model -> Model
tickAnimation time model = { model | animation = Style.tick time model.animation }

handleState : Model -> Maybe Model
handleState model = case model.state of
  Dead  -> Nothing
  Dying -> Just (die model)
  _     -> Just model

die : Model -> Model
die model = { model | state = Dead, animation = Animation.removeTimer }


-- View

view : Model -> Html Msg
view model =
  let animationStyle = Style.render model.animation
  in span [ style animationStyle, class ("timer") ]
          [ viewPlayer model
          , viewRemoveButton
          ]

viewPlayer : Model -> Html Msg
viewPlayer {player} = Player.view player |> App.map Modify

viewRemoveButton : Html Msg
viewRemoveButton =
  let icon cls col = span [ class cls ] [ Icon.remove col 15 ]
      val          = [ icon "icon" Color.black
                     , icon "icon-hover" Color.white
                     , span [ class "text" ] [ text "Remove Timer" ]
                     ]
  in button [ class "btn btn-remove", onClick Kill ] val
