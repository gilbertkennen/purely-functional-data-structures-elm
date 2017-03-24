module Ch2.UnbalancedSetTests exposing (all)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, intRange)
import Misc
import Set
import Ch2.UnbalancedSet as USet


all : Test
all =
    describe "UnbalancedSet"
        [ test "empty trees have size 0" <|
            \() ->
                USet.empty
                    |> USet.size
                    |> Expect.equal 0
        , fuzz int "inserting one item creates a tree with size 1" <|
            \x ->
                USet.empty
                    |> USet.insertWith compare x
                    |> USet.size
                    |> Expect.equal 1
        , fuzz (list int) "inserting many items, those items can be looked up" <|
            \xs ->
                Misc.maybeChain
                    (\x tree ->
                        USet.lookupWith compare x tree
                            |> Maybe.andThen (always (Just tree))
                    )
                    (Just
                        (List.foldl
                            (USet.insertWith compare)
                            USet.empty
                            xs
                        )
                    )
                    xs
                    |> Expect.notEqual Nothing
        , fuzz (list int) "inserting many items creates tree with size of unique items" <|
            \xs ->
                USet.fromList compare xs
                    |> USet.size
                    |> Expect.equal (Set.fromList xs |> Set.size)
        , test "a complete tree of size 0 is Empty" <|
            \() ->
                USet.complete 0 Nothing
                    |> Expect.equal USet.empty
        , fuzz (intRange 0 16) "a complete tree of size n size 2^n - 1" <|
            \n ->
                USet.complete n Nothing
                    |> USet.size
                    |> Expect.equal ((2 ^ n) - 1)
        , fuzz (intRange 0 16) "a complete tree's branches are identical on every level" <|
            \n ->
                Misc.maybeChain
                    (\_ tree ->
                        case tree of
                            USet.Empty ->
                                Just USet.Empty

                            USet.Node left _ right ->
                                if left == right then
                                    Just left
                                else
                                    Nothing
                    )
                    (Just (USet.complete n Nothing))
                    (List.range 1 n)
                    |> Expect.notEqual Nothing
        , test "a balanced tree of size 0 is Empty" <|
            \() ->
                USet.create 0 Nothing
                    |> Expect.equal USet.Empty
        , fuzz (intRange 0 (2 ^ 8))
            "every node in a balanced tree has children whose sizes vary by as much as 1"
          <|
            \n ->
                checkBalance (USet.create n Nothing)
                    |> Expect.true "imbalance found"
        , fuzz (list int) "insert is a convenient insertWith" <|
            \xs ->
                List.foldl (USet.insertWith compare) USet.empty xs
                    |> Expect.equal
                        (List.foldl USet.insert USet.empty xs)
        , fuzz (list int) "member is a convenient lookupWith" <|
            \xs ->
                Misc.maybeChain
                    (\x tree ->
                        if USet.member x tree then
                            Just tree
                        else
                            Nothing
                    )
                    (Just (USet.fromList compare xs))
                    xs
                    |> Expect.notEqual Nothing
        ]


checkBalance : USet.UnbalancedSet a -> Bool
checkBalance tree =
    case tree of
        USet.Empty ->
            True

        USet.Node left _ right ->
            abs (USet.size left - USet.size right)
                <= 1
                && checkBalance left
                && checkBalance right
