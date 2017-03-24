module Ch2.UnbalancedTree
    exposing
        ( UnbalancedTree(Empty, Node)
        , empty
        , insertWith
        , lookupWith
        , complete
        , create
        , size
        , fromList
        )

import Misc exposing (isOdd)


type UnbalancedTree a
    = Empty
    | Node (UnbalancedTree a) a (UnbalancedTree a)


empty : UnbalancedTree a
empty =
    Empty


insertWith : (a -> a -> Order) -> a -> UnbalancedTree a -> UnbalancedTree a
insertWith cmp x xs =
    Maybe.withDefault xs (insertWithHelper cmp x xs)


insertWithHelper : (a -> a -> Order) -> a -> UnbalancedTree a -> Maybe (UnbalancedTree a)
insertWithHelper cmp x xs =
    case xs of
        Empty ->
            Just (Node Empty x Empty)

        Node left y right ->
            case cmp x y of
                LT ->
                    Maybe.map (\l -> Node l y right) (insertWithHelper cmp x left)

                GT ->
                    Maybe.map (\r -> Node left y r) (insertWithHelper cmp x right)

                EQ ->
                    Just (Node left x right)


lookupWith : (a -> b -> Order) -> a -> UnbalancedTree b -> Maybe b
lookupWith cmp x xs =
    case xs of
        Empty ->
            Nothing

        Node left y right ->
            case cmp x y of
                LT ->
                    lookupWith cmp x left

                GT ->
                    lookupWith cmp x right

                EQ ->
                    Just y


complete : Int -> a -> UnbalancedTree a
complete d x =
    if d <= 0 then
        Empty
    else
        let
            leaf =
                complete (d - 1) x
        in
            Node leaf x leaf


create : Int -> a -> UnbalancedTree a
create n x =
    if n == 0 then
        Empty
    else if isOdd n then
        let
            leaf =
                create (n // 2) x
        in
            Node leaf x leaf
    else
        let
            ( left, right ) =
                createHelper ((n - 1) // 2) x
        in
            Node left x right


createHelper : Int -> a -> ( UnbalancedTree a, UnbalancedTree a )
createHelper m x =
    if m == 0 then
        ( Empty, Node Empty x Empty )
    else if isOdd m then
        let
            ( left, right ) =
                createHelper (m // 2) x
        in
            ( Node left x left, Node left x right )
    else
        let
            ( left, right ) =
                createHelper ((m - 1) // 2) x
        in
            ( Node left x right, Node right x right )


size : UnbalancedTree a -> Int
size tree =
    case tree of
        Empty ->
            0

        Node left _ right ->
            1 + size left + size right


fromList : (a -> a -> Order) -> List a -> UnbalancedTree a
fromList f =
    List.foldl (insertWith f) empty
