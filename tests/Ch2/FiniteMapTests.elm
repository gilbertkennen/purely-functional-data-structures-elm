module Ch2.FiniteMapTests exposing (all)

import Test exposing (..)
import Expect
import Fuzz exposing (Fuzzer, tuple, list, int)
import Misc
import Ch2.FiniteMap as FMap


all : Test
all =
    describe "FiniteMap tests"
        [ test "empty FiniteMap should have size of 0" <|
            \() ->
                FMap.empty
                    |> FMap.size
                    |> Expect.equal 0
        , fuzz pairs "binding many items is the same as fromList" <|
            \xs ->
                Expect.equal
                    (FMap.fromList xs)
                    (List.foldl (\( k, v ) map -> FMap.bind k v map) FMap.empty xs)
        , fuzz pairs "binding many items should allow them to be looked up" <|
            \xs ->
                Misc.maybeChain
                    (\( k, _ ) map ->
                        FMap.lookup k map |> Maybe.map (always map)
                    )
                    (Just (FMap.fromList xs))
                    xs
                    |> Expect.notEqual Nothing
        , fuzz (tuple ( pair, pair )) "last bind wins" <|
            \( ( lk, lv ), ( rk, rv ) ) ->
                FMap.empty
                    |> FMap.bind lk lv
                    |> FMap.bind rk rv
                    |> FMap.lookup rk
                    |> Expect.equal (Just rv)
        ]


pairs : Fuzzer (List ( Int, Int ))
pairs =
    list pair


pair : Fuzzer ( Int, Int )
pair =
    tuple ( int, int )
