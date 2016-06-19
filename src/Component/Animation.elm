module Component.Animation exposing (addTimer, removeTimer)

import Style
import Style.Properties exposing (..)

addTimer : Style.Animation
addTimer = Style.queue
        |> Style.to timerAnimation.open
        |> Style.on (Style.init timerAnimation.close)

removeTimer : Style.Animation
removeTimer = Style.queue
           |> Style.to timerAnimation.close
           |> Style.andThen
           |> Style.set [ Display None ]
           |> Style.on (Style.init timerAnimation.open)

timerAnimation =
  { close =  [ Height 0 Px
             , Opacity 0
             , PaddingBottom 0 Em
             , MarginTop 0 Em
             , Display InlineBlock
             ]
  , open = [ Height 120 Px
          , Opacity 1
          , PaddingBottom 0.8 Em
          , MarginTop 0.8 Em
          , Display InlineBlock
          ]
  }
