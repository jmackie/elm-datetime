module Clock.Hour exposing
    ( Hour
    , fromInt, fromPosix
    , zero, midnight, Period, period
    , increment, add, compare
    , toInt
    )

{-| The hour component of a clock time.


# Type definition

@docs Hour


# Creating values

@docs fromInt, fromPosix


# Constants and helpers

@docs zero, midnight, Period, period


# Operations

@docs increment, add, compare


# Conversions

@docs toInt

-}

import Time


{-| The hour component of a clock time.

Internally, `Hour` is represented as an integer with a lower bound of 0 and
an upper bound of 23 (inclusive).

-}
type Hour
    = Hour Int


{-| Attempt to construct an `Hour` from an `Int`.

    > fromInt 2
    Just (Hour 2) : Maybe Hour

    > fromInt 10
    Just (Hour 10) : Maybe Hour

    > fromInt 25
    Nothing : Maybe Hour

-}
fromInt : Int -> Maybe Hour
fromInt int =
    if isValid int then
        Just (Hour int)

    else
        Nothing


{-| Get an `Hour` from a time zone and posix time.

    > fromPosix Time.utc (Time.millisToPosix 0)
    Hour 0 : Hour

-}
fromPosix : Time.Zone -> Time.Posix -> Hour
fromPosix zone posix =
    -- We trust the Time package...
    Hour (Time.toHour zone posix)


isValid : Int -> Bool
isValid int =
    int >= 0 && int <= 23


{-| Convert an `Hour` to an `Int`.

    > toInt midnight
    0 : Int

    > fromInt 21 |> Maybe.map toInt
    Just 21 : Maybe Int

-}
toInt : Hour -> Int
toInt (Hour int) =
    int


{-| Zero hours.

    > zero
    Hour 0 : Hour

-}
zero : Hour
zero =
    Hour 0


{-| 12am (alias for `zero`).

    > midnight
    Hour 0 : Hour

-}
midnight : Hour
midnight =
    zero


{-| Add two `Hour` values together. Returns `Nothing` if the resulting `Hour` would be invalid.
-}
add : Hour -> Hour -> Maybe Hour
add (Hour lhs) (Hour rhs) =
    fromInt (lhs + rhs)


{-| Increment an `Hour`. Returns `Nothing` if the resulting `Hour` would be invalid.

    > increment midnight
    Just (Hour 1) : Maybe Hour

-}
increment : Hour -> Maybe Hour
increment (Hour int) =
    fromInt (int + 1)


{-| Compare two `Hour` values.

    > Clock.Hour.compare midnight midnight
    EQ : Order

-}
compare : Hour -> Hour -> Order
compare lhs rhs =
    Basics.compare (toInt lhs) (toInt rhs)


{-| Periods of a 12-hour clock.
-}
type Period
    = AM
    | PM


{-| Get the `Period` of a given hour.
-}
period : Hour -> Period
period (Hour int) =
    if int < 12 then
        AM

    else
        PM
