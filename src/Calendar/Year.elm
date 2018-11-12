module Calendar.Year exposing
    ( Year
    , fromInt, fromPosix
    , zero, isLeapYear
    , increment, add, compare
    , toInt
    )

{-| The year component of a date.


# Type definition

@docs Year


# Creating values

@docs fromInt, fromPosix


# Constants and helpers

@docs zero, isLeapYear


# Operations

@docs increment, add, compare


# Conversions

@docs toInt

-}

import Time


{-| The year component of a (Gregorian) calendar date.

Internally, `Year` is represented as an integer with a lower bound of 0.

-}
type Year
    = Year Int


{-| Attempt to construct a `Year` from an `Int`.

    > fromInt 2018
    Just (Year 2018) : Maybe Year

    > fromInt -20
    Nothing : Maybe Year

-}
fromInt : Int -> Maybe Year
fromInt int =
    if isValid int then
        Just (Year int)

    else
        Nothing


isValid : Int -> Bool
isValid int =
    int > 0


{-| Get a `Year` from a time zone and posix time.

    > fromPosix Time.utc (Time.millisToPosix 0)
    Year 1970 : Year

-}
fromPosix : Time.Zone -> Time.Posix -> Year
fromPosix zone posix =
    -- We trust the Time package...
    Year (Time.toYear zone posix)


{-| Add two years together.

    > add (increment zero) (increment zero)
    Year 2 : Year

-}
add : Year -> Year -> Year
add (Year lhs) (Year rhs) =
    Year (lhs + rhs)


{-| Convert a `Year` to an `Int`.

    > toInt zero
    0 : Int

    > fromInt 2018 |> Maybe.map toInt
    Just 2018 : Maybe Int

-}
toInt : Year -> Int
toInt (Year int) =
    int


{-| `Year` zero.

    > zero
    Year 0 : Year

-}
zero : Year
zero =
    Year 0


{-| Increment a `Year`.

    > increment zero
    Year 1 : Year

-}
increment : Year -> Year
increment (Year int) =
    Year (int + 1)


{-| Compare two `Year` values.
-}
compare : Year -> Year -> Order
compare lhs rhs =
    Basics.compare (toInt lhs) (toInt rhs)


{-| Was this year a leap year?
-}
isLeapYear : Year -> Bool
isLeapYear (Year int) =
    (modBy 4 int == 0) && ((modBy 400 int == 0) || not (modBy 100 int == 0))
