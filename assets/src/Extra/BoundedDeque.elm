module Extra.BoundedDeque exposing (clear)

import BoundedDeque exposing (BoundedDeque)


clear : BoundedDeque a -> BoundedDeque a
clear bd =
    BoundedDeque.empty (BoundedDeque.getMaxSize bd)
