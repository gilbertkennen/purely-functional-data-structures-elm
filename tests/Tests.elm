module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import Ch2.LinkedListTests
import Ch2.UnbalancedTreeTests


all : Test
all =
    describe "All tests."
        [ Ch2.LinkedListTests.all
        , Ch2.UnbalancedTreeTests.all
        ]
