module Calendar.Month exposing
    ( Month
    , fromInt, fromPosix
    , compare, next
    , toInt
    , months
    )

{-| The month component of a date.


# Type definition

@docs Month


# Creating values

@docs fromInt, fromPosix


# Operations

@docs compare, next


# Conversions

@docs toInt


# Extras

@docs months

-}

import Time


{-| The month component of a (Gregorian) calendar date.
-}
type alias Month =
    Time.Month


{-| Attempt to create a `Month` from an `Int`.

    > fromInt 1
    Just Jan : Maybe Month

    > fromInt 10
    Just Oct : Maybe Month

    > fromInt 42
    Nothing : Maybe Month

-}
fromInt : Int -> Maybe Month
fromInt int =
    case int of
        1 ->
            Just Time.Jan

        2 ->
            Just Time.Feb

        3 ->
            Just Time.Mar

        4 ->
            Just Time.Apr

        5 ->
            Just Time.May

        6 ->
            Just Time.Jun

        7 ->
            Just Time.Jul

        8 ->
            Just Time.Aug

        9 ->
            Just Time.Sep

        10 ->
            Just Time.Oct

        11 ->
            Just Time.Nov

        12 ->
            Just Time.Dec

        _ ->
            Nothing


{-| Get a `Month` from a time zone and posix time.

    > fromPosix Time.utc (Time.millisToPosix 0)
    Jan : Month

-}
fromPosix : Time.Zone -> Time.Posix -> Month
fromPosix =
    Time.toMonth


{-| Convert a `Month` to an `Int`.

    > toInt Time.Jan
    1 : Int

    > toInt Time.Dec
    12 : Int

-}
toInt : Month -> Int
toInt month =
    case month of
        Time.Jan ->
            1

        Time.Feb ->
            2

        Time.Mar ->
            3

        Time.Apr ->
            4

        Time.May ->
            5

        Time.Jun ->
            6

        Time.Jul ->
            7

        Time.Aug ->
            8

        Time.Sep ->
            9

        Time.Oct ->
            10

        Time.Nov ->
            11

        Time.Dec ->
            12


{-| Compare two `Month` values.

    > Calendar.Month.compare Time.Feb Time.Jan
    GT : Order

-}
compare : Month -> Month -> Order
compare lhs rhs =
    Basics.compare (toInt lhs) (toInt rhs)


{-| Move to the next `Month`.

    > next Time.Jan
    Feb : Month

    > next Time.Dec
    Jan : Month

-}
next : Month -> Month
next month =
    case month of
        Time.Jan ->
            Time.Feb

        Time.Feb ->
            Time.Mar

        Time.Mar ->
            Time.Apr

        Time.Apr ->
            Time.May

        Time.May ->
            Time.Jun

        Time.Jun ->
            Time.Jul

        Time.Jul ->
            Time.Aug

        Time.Aug ->
            Time.Sep

        Time.Sep ->
            Time.Oct

        Time.Oct ->
            Time.Nov

        Time.Nov ->
            Time.Dec

        Time.Dec ->
            Time.Jan


{-| All the months.
-}
months : List Month
months =
    [ Time.Jan
    , Time.Feb
    , Time.Mar
    , Time.Apr
    , Time.May
    , Time.Jun
    , Time.Jul
    , Time.Aug
    , Time.Sep
    , Time.Oct
    , Time.Nov
    , Time.Dec
    ]
