module Ch2.LinkedList
    exposing
        ( LinkedList(Empty, LinkedList)
        , empty
        , isEmpty
        , cons
        , head
        , tail
        , cat
        , update
        , suffixes
        , length
        , fromList
        , drop
        , take
        )


type LinkedList a
    = Empty
    | LinkedList a (LinkedList a)


empty : LinkedList a
empty =
    Empty


isEmpty : LinkedList a -> Bool
isEmpty xs =
    case xs of
        Empty ->
            True

        LinkedList _ _ ->
            False


cons : a -> LinkedList a -> LinkedList a
cons =
    LinkedList


head : LinkedList a -> Maybe a
head xs =
    case xs of
        Empty ->
            Nothing

        LinkedList x _ ->
            Just x


tail : LinkedList a -> Maybe (LinkedList a)
tail xs =
    case xs of
        Empty ->
            Nothing

        LinkedList _ xss ->
            Just xss


cat : LinkedList a -> LinkedList a -> LinkedList a
cat left right =
    case left of
        Empty ->
            right

        LinkedList x xs ->
            cons x (cat xs right)


update : Int -> a -> LinkedList a -> Maybe (LinkedList a)
update i x xs =
    case xs of
        Empty ->
            Nothing

        LinkedList y ys ->
            if i == 0 then
                Just (cons x ys)
            else
                update (i - 1) x ys
                    |> Maybe.map (cons y)


suffixes : LinkedList a -> LinkedList (LinkedList a)
suffixes xs =
    case xs of
        Empty ->
            cons Empty Empty

        LinkedList _ ys ->
            cons xs (suffixes ys)


length : LinkedList a -> Int
length xs =
    case xs of
        Empty ->
            0

        LinkedList _ ys ->
            1 + (length ys)


fromList : List a -> LinkedList a
fromList =
    List.foldl cons empty << List.reverse


drop : Int -> LinkedList a -> LinkedList a
drop n xs =
    if n == 0 then
        xs
    else
        case xs of
            Empty ->
                Empty

            LinkedList _ ys ->
                drop (n - 1) ys


take : Int -> LinkedList a -> LinkedList a
take n xs =
    if n == 0 then
        Empty
    else
        case xs of
            Empty ->
                Empty

            LinkedList y ys ->
                LinkedList y (take (n - 1) ys)
