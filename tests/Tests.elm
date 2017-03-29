module Tests exposing (..)

import Test exposing (..)
import Ch2.LinkedListTests
import Ch2.UnbalancedSetTests
import Ch2.FiniteMapTests


all : Test
all =
    describe "All tests."
        [ Ch2.LinkedListTests.all
        , Ch2.UnbalancedSetTests.all
        , Ch2.FiniteMapTests.all
        ]
