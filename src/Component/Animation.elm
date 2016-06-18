module Component.Animation exposing (..)

import Style
import Style.Properties exposing (..)

addTimer : Style.Animation
addTimer =
  let initial = Style.init [ TranslateY -100 Px, Opacity 0]
  in Style.animate
  |> Style.to [ TranslateY 0 Px, Opacity 1 ]
  |> Style.on initial
