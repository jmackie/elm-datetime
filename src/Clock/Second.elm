module Clock.Second exposing
    ( Second
    , zero
    , fromInt, fromPosix
    , increment, add, compare
    , toInt
    )

{-| The second component of a clock time.


# Type definition

@docs Second


# Constants

@docs zero


# Creating values

@docs fromInt, fromPosix


# Operations

@docs increment, add, compare


# Conversions

@docs toInt

-}

import Time


{-| The second component of a clock time.

Internally, `Second` is represented as an integer with a lower bound of 0 and
an upper bound of 59 (inclusive).

-}
type Second
    = Second Int


{-| Attempt to construct a `Second` from an `Int`.

    > fromInt 3
    Just (Second 2) : Maybe Second

    > fromInt 45
    Just (Second 45) : Maybe Second

    > fromInt 75
    Nothing : Maybe Second

-}
fromInt : Int -> Maybe Second
fromInt int =
    if isValid int then
        Just (Second int)

    else
        Nothing


{-| Get a `Second` from a time zone and posix time.

    > fromPosix Time.utc (Time.millisToPosix 0)
    Second 0 : Second

-}
fromPosix : Time.Zone -> Time.Posix -> Second
fromPosix zone posix =
    -- We trust the Time package...
    Second (Time.toSecond zone posix)


isValid : Int -> Bool
isValid int =
    int >= 0 && int <= 59


{-| Convert a `Second` to an `Int`.

    > toInt zero
    0 : Int

    > fromInt 50 |> Maybe.map toInt
    Just 50 : Maybe Int

-}
toInt : Second -> Int
toInt (Second int) =
    int


{-| Second zero.

    > zero
    Second 0 : Second

-}
zero : Second
zero =
    Second 0


{-| Add two `Second` values together. Returns `Nothing` if the result would be invalid.
-}
add : Second -> Second -> Maybe Second
add (Second lhs) (Second rhs) =
    fromInt (lhs + rhs)


{-| Increment a `Second`. Returns `Nothing` if the result would be invalid.

    > increment zero
    Just (Second 1) : Maybe Second

-}
increment : Second -> Maybe Second
increment (Second int) =
    fromInt (int + 1)


{-| Compare two `Second` values.

    > Clock.Second.compare zero zero
    EQ : Order

-}
compare : Second -> Second -> Order
compare lhs rhs =
    Basics.compare (toInt lhs) (toInt rhs)
