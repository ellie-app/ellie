module Shared.Selector exposing (selector)

import Native.Selector


selector : (a -> b) -> (a -> b)
selector fn =
    Native.Selector.memoize fn
