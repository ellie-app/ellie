module Extra.Debug exposing (..)

import Native.Debug


break : a -> a
break =
    Native.Debug.break
