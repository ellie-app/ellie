module Tests exposing (..)

import Test exposing (..)
import Data.Extra.ListTests
import Data.Extra.EveryDictTests


all : Test
all =
    describe "All Tests"
        [ Data.Extra.ListTests.tests
        , Data.Extra.EveryDictTests.tests
        ]
