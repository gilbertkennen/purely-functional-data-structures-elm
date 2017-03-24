module Ch2.UnbalancedTreeTests exposing (all)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, intRange)
import Set
import Ch2.UnbalancedTree as Tree


all : Test
all =
    describe "UnbalancedTree"
        [ test "empty trees have size 0" <|
            \() ->
                Tree.empty
                    |> Tree.size
                    |> Expect.equal 0
        , fuzz int "inserting one item creates a tree with size 1" <|
            \x ->
                Tree.empty
                    |> Tree.insertWith compare x
                    |> Tree.size
                    |> Expect.equal 1
        , fuzz (list int) "inserting many items, those items can be looked up" <|
            \xs ->
                maybeChain
                    (\x tree ->
                        Tree.lookupWith compare x tree
                            |> Maybe.andThen (always (Just tree))
                    )
                    (Just
                        (List.foldl
                            (Tree.insertWith compare)
                            Tree.empty
                            xs
                        )
                    )
                    xs
                    |> Expect.notEqual Nothing
        , fuzz (list int) "inserting many items creates tree with size of unique items" <|
            \xs ->
                Tree.fromList compare xs
                    |> Tree.size
                    |> Expect.equal (Set.fromList xs |> Set.size)
        , test "a complete tree of size 0 is Empty" <|
            \() ->
                Tree.complete 0 Nothing
                    |> Expect.equal Tree.empty
        , fuzz (intRange 0 16) "a complete tree of size n size 2^n - 1" <|
            \n ->
                Tree.complete n Nothing
                    |> Tree.size
                    |> Expect.equal ((2 ^ n) - 1)
        , fuzz (intRange 0 16) "a complete tree's branches are identical on every level" <|
            \n ->
                maybeChain
                    (\_ tree ->
                        case tree of
                            Tree.Empty ->
                                Just Tree.Empty

                            Tree.Node left _ right ->
                                if left == right then
                                    Just left
                                else
                                    Nothing
                    )
                    (Just (Tree.complete n Nothing))
                    (List.range 1 n)
                    |> Expect.notEqual Nothing
        , test "a balanced tree of size 0 is Empty" <|
            \() ->
                Tree.create 0 Nothing
                    |> Expect.equal Tree.Empty
        , fuzz (intRange 0 (2 ^ 8))
            "every node in a balanced tree has children whose sizes vary by as much as 1"
          <|
            \n ->
                checkBalance (Tree.create n Nothing)
                    |> Expect.true "imbalance found"
        ]


maybeChain : (a -> b -> Maybe b) -> Maybe b -> List a -> Maybe b
maybeChain f =
    List.foldl (Maybe.andThen << f)


checkBalance : Tree.UnbalancedTree a -> Bool
checkBalance tree =
    case tree of
        Tree.Empty ->
            True

        Tree.Node left _ right ->
            abs (Tree.size left - Tree.size right)
                <= 1
                && checkBalance left
                && checkBalance right
