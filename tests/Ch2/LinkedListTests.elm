module Ch2.LinkedListTests exposing (all)

import Ch2.LinkedList as LL
import Misc
import Test exposing (..)
import Expect
import Fuzz exposing (Fuzzer, list, int, tuple)


all : Test
all =
    describe "all LinkedList tests"
        [ test "empty LinkedList has length zero" <|
            \() ->
                LL.empty
                    |> LL.length
                    |> Expect.equal 0
        , fuzz int "cons on an empty list has length 1" <|
            \x ->
                LL.empty
                    |> LL.cons x
                    |> LL.length
                    |> Expect.equal 1
        , fuzz (list int) "cons on n elements has length n" <|
            \xs ->
                List.foldl LL.cons LL.empty xs
                    |> LL.length
                    |> Expect.equal (List.length xs)
        , test "tail of an empty LinkedList is Nothing" <|
            \() ->
                LL.empty
                    |> LL.tail
                    |> maybeEqual Nothing
        , fuzz int "single element linked list has tail isEmpty" <|
            \x ->
                LL.empty
                    |> LL.cons x
                    |> LL.tail
                    |> maybeEqual (Just LL.empty)
        , fuzz (list int) "length(tail) of a LinkedList of length n is Just (n-1) or Nothing" <|
            \xs ->
                case xs of
                    [] ->
                        LL.empty
                            |> LL.tail
                            |> maybeEqual Nothing

                    _ ->
                        LL.fromList xs
                            |> LL.tail
                            |> Maybe.map LL.length
                            |> maybeEqual (Just (List.length xs - 1))
        , test "head of an empty LinkedList is Nothing" <|
            \() ->
                LL.empty
                    |> LL.head
                    |> maybeEqual Nothing
        , fuzz int "head of a single-element LinkedList is Just x" <|
            \x ->
                LL.empty
                    |> LL.cons x
                    |> LL.head
                    |> maybeEqual (Just x)
        , fuzz (list int) "cons and head are (nearly) inverses" <|
            \xs ->
                Misc.scanlWith
                    (\_ ll ->
                        ( Maybe.andThen LL.head ll
                        , Maybe.andThen LL.tail ll
                        )
                    )
                    (Just <| LL.fromList xs)
                    (List.range 1 (List.length xs))
                    |> List.filterMap identity
                    |> Expect.equal xs
        , fuzz (listAndUnreliableIndex int) "dropping n from a LinkedList drops from the head" <|
            \( xs, n ) ->
                LL.fromList xs
                    |> LL.drop n
                    |> Expect.equal (List.drop n xs |> LL.fromList)
        , fuzz (listAndUnreliableIndex int) "taking n from a LinkedList takes from the head" <|
            \( xs, n ) ->
                LL.fromList xs
                    |> LL.take n
                    |> Expect.equal (List.take n xs |> LL.fromList)
        , fuzz (tuple ( (list int), (list int) )) "cat two LinkedList puts the left before the right" <|
            \( left, right ) ->
                (LL.cat (LL.fromList left) (LL.fromList right))
                    |> Expect.equal (LL.fromList (left ++ right))
        , fuzz (tuple ( listAndUnreliableIndex int, int )) "update sets the item at index n" <|
            \( ( xs, i ), x ) ->
                LL.fromList xs
                    |> LL.update i x
                    |> Maybe.map (LL.drop i)
                    |> Maybe.andThen LL.head
                    |> maybeEqual
                        (if i < List.length xs then
                            Just x
                         else
                            Nothing
                        )
        , fuzz (tuple ( listAndIndex int, int )) "update does not alter items before the index" <|
            \( ( xs, i ), x ) ->
                LL.fromList xs
                    |> LL.update i x
                    |> Maybe.map (LL.take i)
                    |> maybeEqual
                        (if List.length xs == 0 then
                            Nothing
                         else
                            Just
                                (List.take i xs
                                    |> LL.fromList
                                )
                        )
        , fuzz (tuple ( listAndIndex int, int )) "update does not alter items after the index" <|
            \( ( xs, i ), x ) ->
                LL.fromList xs
                    |> LL.update i x
                    |> Maybe.map (LL.drop (i + 1))
                    |> maybeEqual
                        (if List.length xs == 0 then
                            Nothing
                         else
                            Just
                                (List.drop (i + 1) xs
                                    |> LL.fromList
                                )
                        )
        , fuzz (llFuzz int) "with i and j in suffixes, j is the tail of i" <|
            \xs ->
                xs
                    |> LL.suffixes
                    |> Expect.true "j is not the tail of i"
                    << checkTails
        ]


checkTails : LL.LinkedList (LL.LinkedList a) -> Bool
checkTails xs =
    case xs of
        LL.Empty ->
            True

        LL.LinkedList (LL.Empty) (LL.Empty) ->
            True

        LL.LinkedList (LL.LinkedList _ ys) (LL.LinkedList z zs) ->
            ys == z && checkTails (LL.LinkedList z zs)

        _ ->
            False


listAndIndex : Fuzzer a -> Fuzzer ( List a, Int )
listAndIndex =
    listAndIndexHelper (\len -> len - 1)


listAndUnreliableIndex : Fuzzer a -> Fuzzer ( List a, Int )
listAndUnreliableIndex =
    listAndIndexHelper (\len -> len + (len // 8))


listAndIndexHelper : (Int -> Int) -> Fuzzer a -> Fuzzer ( List a, Int )
listAndIndexHelper f fuzz =
    (list fuzz)
        |> Fuzz.andThen
            (\xs ->
                let
                    len =
                        List.length xs
                in
                    tuple ( Fuzz.constant xs, Fuzz.intRange 0 (f len) )
            )


llFuzz : Fuzzer a -> Fuzzer (LL.LinkedList a)
llFuzz fuzz =
    Fuzz.map LL.fromList (list fuzz)


maybeEqual : Maybe a -> Maybe a -> Expect.Expectation
maybeEqual left right =
    case ( left, right ) of
        ( Nothing, Nothing ) ->
            Expect.pass

        ( Just a, Just b ) ->
            Expect.equal a b

        _ ->
            Expect.fail "Nothing and Just _ can't be equal."
