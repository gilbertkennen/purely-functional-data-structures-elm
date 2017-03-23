port module Main exposing (..)

import Ch2.LinkedListTests
import Test.Runner.Node exposing (run, TestProgram)
import Json.Encode exposing (Value)


main : TestProgram
main =
    run emit Ch2.LinkedListTests.all


port emit : ( String, Value ) -> Cmd msg
