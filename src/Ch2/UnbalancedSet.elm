module Ch2.UnbalancedSet
    exposing
        ( UnbalancedSet(Empty, Node)
        , empty
        , insert
        , member
        , insertWith
        , lookupWith
        , complete
        , create
        , size
        , fromList
        )

import Misc exposing (isOdd)


type UnbalancedSet a
    = Empty
    | Node (UnbalancedSet a) a (UnbalancedSet a)


insert : comparable -> UnbalancedSet comparable -> UnbalancedSet comparable
insert x xs =
    insertWith compare x xs


member : comparable -> UnbalancedSet comparable -> Bool
member x xs =
    case lookupWith compare x xs of
        Nothing ->
            False

        Just _ ->
            True


empty : UnbalancedSet a
empty =
    Empty


insertWith : (a -> a -> Order) -> a -> UnbalancedSet a -> UnbalancedSet a
insertWith cmp x xs =
    Maybe.withDefault xs (insertWithHelper cmp x xs)


insertWithHelper : (a -> a -> Order) -> a -> UnbalancedSet a -> Maybe (UnbalancedSet a)
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


lookupWith : (a -> b -> Order) -> a -> UnbalancedSet b -> Maybe b
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


complete : Int -> a -> UnbalancedSet a
complete d x =
    if d <= 0 then
        Empty
    else
        let
            leaf =
                complete (d - 1) x
        in
            Node leaf x leaf


create : Int -> a -> UnbalancedSet a
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


createHelper : Int -> a -> ( UnbalancedSet a, UnbalancedSet a )
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


size : UnbalancedSet a -> Int
size tree =
    case tree of
        Empty ->
            0

        Node left _ right ->
            1 + size left + size right


fromList : (a -> a -> Order) -> List a -> UnbalancedSet a
fromList f =
    List.foldl (insertWith f) empty
