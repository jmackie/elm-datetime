module Clock.Minute exposing
    ( Minute
    , zero
    , fromInt, fromPosix
    , increment, add, compare
    , toInt
    )

{-| The minute component of a clock time.


# Type definition

@docs Minute


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


{-| The minute component of a clock time.

Internally, `Minute` is represented as an integer with a lower bound of 0 and
an upper bound of 59 (inclusive).

-}
type Minute
    = Minute Int


{-| Attempt to construct a `Minute` from an `Int`.

    > fromInt 3
    Just (Minute 2) : Maybe Minute

    > fromInt 45
    Just (Minute 45) : Maybe Minute

    > fromInt 75
    Nothing : Maybe Minute

-}
fromInt : Int -> Maybe Minute
fromInt int =
    if isValid int then
        Just (Minute int)

    else
        Nothing


{-| Get a `Minute` from a time zone and posix time.

    > fromPosix Time.utc (Time.millisToPosix 0)
    Minute 0 : Minute

-}
fromPosix : Time.Zone -> Time.Posix -> Minute
fromPosix zone posix =
    -- We trust the Time package...
    Minute (Time.toMinute zone posix)


isValid : Int -> Bool
isValid int =
    int >= 0 && int <= 59


{-| Convert a `Minute` to an `Int`.

    > toInt zero
    0 : Int

    > fromInt 50 |> Maybe.map toInt
    Just 50 : Maybe Int

-}
toInt : Minute -> Int
toInt (Minute int) =
    int


{-| Minute zero.

    > zero
    Minute 0 : Minute

-}
zero : Minute
zero =
    Minute 0


{-| Add two `Minute` values together. Returns `Nothing` if the result would be invalid.
-}
add : Minute -> Minute -> Maybe Minute
add (Minute lhs) (Minute rhs) =
    fromInt (lhs + rhs)


{-| Increment a `Minute`. Returns `Nothing` if the result would be invalid.

    > increment zero
    Just (Minute 1) : Maybe Minute

-}
increment : Minute -> Maybe Minute
increment (Minute int) =
    fromInt (int + 1)


{-| Compare two `Minute` values.

    > Clock.Minute.compare zero zero
    EQ : Order

-}
compare : Minute -> Minute -> Order
compare lhs rhs =
    Basics.compare (toInt lhs) (toInt rhs)
