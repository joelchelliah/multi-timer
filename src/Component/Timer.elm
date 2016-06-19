module Component.Timer exposing ( Model, Msg, init, update, view
                                , tick, tickAnimation, handleState )

import Component.Player    as Player
import Component.Duration  as Duration
import Component.Animation as Animation

import Html.App as App
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Style
import Color
import FontAwesome as Icon


-- Model

type alias Model = { duration : Duration.Model
                   , player : Player.Model
                   , state : State
                   , animation: Style.Animation
                   , id : Int
                   }

type State = Alive
           | Dying
           | Dead

init : Int -> Model
init = initWithDuration Duration.init

initWithDuration : Duration.Model -> Int -> Model
initWithDuration duration =
  Model duration Player.init Alive Animation.addTimer


-- Update

type Msg = Kill
         | ModifyDuration Duration.Msg
         | ModifyPlayer Player.Msg

update : Msg -> Model -> Model
update msg ({duration, player} as model) =
  case msg of
    Kill               -> { model | state    = Dying }
    ModifyDuration msg -> { model | duration = Duration.update msg duration }
    ModifyPlayer   msg -> { model | player   = if Duration.isZero duration
                                               then Player.init
                                               else Player.update msg player
                          }

tick : Model -> Model
tick ({duration, player} as model) =
  if Duration.isZero duration
  then { model | player = Player.init }
  else if Player.isPlaying player
       then tickDuration model
       else model

tickDuration : Model -> Model
tickDuration model =
  { model | duration = Duration.tick model.duration }

tickAnimation : Float -> Model -> Model
tickAnimation time model =
  { model | animation = Style.tick time model.animation }

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
          [ viewDuration model
          , viewPlayer model
          , viewRemoveButton
          ]

viewDuration : Model -> Html Msg
viewDuration {duration, player} =
  let enabled = Player.isPlaying player
  in Duration.view duration enabled |> App.map ModifyDuration

viewPlayer : Model -> Html Msg
viewPlayer {player} =
  Player.view player |> App.map ModifyPlayer

viewRemoveButton : Html Msg
viewRemoveButton =
  let icon cls col = span [ class cls ] [ Icon.remove col 20 ]
      val          = [ icon "icon" Color.black
                     , icon "icon-hover" Color.white
                     , span [ class "text" ] [ text "Remove Timer" ]
                     ]
  in button [ class "btn btn-remove", onClick Kill ] val
