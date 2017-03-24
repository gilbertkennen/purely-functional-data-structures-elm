module Ch2.FiniteMap
    exposing
        ( FiniteMap
        , empty
        , bind
        , lookup
        , size
        , fromList
        )

import Ch2.UnbalancedSet as USet


type alias FiniteMap comparable a =
    USet.UnbalancedSet ( comparable, a )


type alias Value comparable a =
    ( comparable, a )


empty : FiniteMap k a
empty =
    USet.Empty


bind : comparable -> a -> FiniteMap comparable a -> FiniteMap comparable a
bind key val map =
    USet.insertWith tupleCompare ( key, val ) map


lookup : comparable -> FiniteMap comparable a -> Maybe a
lookup key map =
    USet.lookupWith (\l ( r, _ ) -> compare l r) key map
        |> Maybe.map Tuple.second


size : FiniteMap comparable a -> Int
size =
    USet.size


fromList : List ( comparable, a ) -> FiniteMap comparable a
fromList =
    USet.fromList tupleCompare


tupleCompare : ( comparable, a ) -> ( comparable, b ) -> Order
tupleCompare ( l, _ ) ( r, _ ) =
    compare l r
