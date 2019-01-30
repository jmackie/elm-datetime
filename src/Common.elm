module Common exposing
    ( appendOrder
    , applyMaybe
    , boundedInt
    , comparing
    , foldOrders
    )


applyMaybe : Maybe a -> Maybe (a -> b) -> Maybe b
applyMaybe a =
    Maybe.andThen (\f -> Maybe.map f a)


type alias Bound =
    { lower : Int
    , upper : Int
    }


boundedInt : (Int -> a) -> Bound -> Int -> Maybe a
boundedInt f bound i =
    if i >= bound.lower && i < bound.upper then
        Just (f i)

    else
        Nothing


comparing : (a -> comparable) -> a -> a -> Order
comparing f lhs rhs =
    compare (f lhs) (f rhs)


foldOrders : Order -> List Order -> Order
foldOrders =
    List.foldl (\order accum -> appendOrder accum order)


appendOrder : Order -> Order -> Order
appendOrder lhs rhs =
    case lhs of
        EQ ->
            rhs

        _ ->
            lhs
