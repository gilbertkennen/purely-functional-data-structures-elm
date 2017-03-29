module Misc
    exposing
        ( scanlWith
        , isOdd
        , maybeChain
        )


isOdd : Int -> Bool
isOdd n =
    n % 2 == 1


scanlWith : (a -> b -> ( c, b )) -> b -> List a -> List c
scanlWith f init xs =
    List.foldl
        (\a ( b, acc ) ->
            let
                ( this, next ) =
                    f a b
            in
                ( next, this :: acc )
        )
        ( init, [] )
        xs
        |> Tuple.second
        |> List.reverse


maybeChain : (a -> b -> Maybe b) -> Maybe b -> List a -> Maybe b
maybeChain f =
    List.foldl (Maybe.andThen << f)
