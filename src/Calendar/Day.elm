module Calendar.Day exposing
    ( Day
    , fromInt, fromPosix, parse
    , one, lastDayOf
    , Format(..)
    , increment, add, compare
    , toString, toInt
    )

{-| The day component of a date.


# Type definition

@docs Day


# Creating values

@docs fromInt, fromPosix, parse


# Constants and helpers

@docs one, lastDayOf


# Formatting

@docs Format


# Operations

@docs increment, add, compare


# Converting to other things

@docs toString, toInt

-}

import Calendar.Month as Month exposing (Month)
import Calendar.Year as Year exposing (Year)
import Parser exposing (Parser)
import Time


{-| The day component of a (Gregorian) calendar date.

Internally, `Day` is represented as an integer with a lower bound of 1 and
an upper bound of 31 (inclusive).

-}
type Day
    = Day Int


{-| Attempt to construct a `Day` from an `Int`.

    > fromInt 15
    Just (Day 15) : Maybe Day

    > fromInt 31
    Just (Day 31) : Maybe Day

    > fromInt 42
    Nothing : Maybe Day

-}
fromInt : Int -> Maybe Day
fromInt int =
    if isValid int then
        Just (Day int)

    else
        Nothing


{-| Get a `Day` from a time zone and posix time.

    > fromPosix Time.utc (Time.millisToPosix 42)
    Day 1 : Day

-}
fromPosix : Time.Zone -> Time.Posix -> Day
fromPosix zone posix =
    -- We trust the Time package...
    Day (Time.toDay zone posix)


isValid : Int -> Bool
isValid int =
    int >= 1 && int <= 31


{-| Convert a `Day` to an `Int`.

    > toInt one
    1 : Int

    > fromInt 21 |> Maybe.map toInt
    Just 21 : Maybe Int

-}
toInt : Day -> Int
toInt (Day int) =
    int


{-| Add two days together. Returns `Nothing` if the resulting `Day` would be invalid.

    > add one one
    Just (Day 2) : Maybe Day

-}
add : Day -> Day -> Maybe Day
add (Day lhs) (Day rhs) =
    fromInt (lhs + rhs)


{-| `Day` one.

    > toInt one
    1 : Int

-}
one : Day
one =
    Day 1


{-| Increment a `Day`. Returns `Nothing` if the resulting `Day` would be invalid.

    > increment one
    Just (Day 2) : Maybe Day

-}
increment : Day -> Maybe Day
increment (Day int) =
    fromInt (int + 1)


{-| Compare two `Day` values.
-}
compare : Day -> Day -> Order
compare lhs rhs =
    Basics.compare (toInt lhs) (toInt rhs)


{-| Ways in which a `Day` can be represented as a `String`.

  - `TwoDigitsFormat`: format as an integer, zero-padded to length two.
    E.g. "02" or "12".

See `toString` examples.

-}
type Format
    = TwoDigitsFormat


{-| Convert a `Day` to a `String` with the given format.

    > toString TwoDigitsFormat one
    "01" : String

-}
toString : Format -> Day -> String
toString format =
    case format of
        TwoDigitsFormat ->
            toStringTwoDigitsFormat


toStringTwoDigitsFormat : Day -> String
toStringTwoDigitsFormat (Day int) =
    zeroPad 2 (String.fromInt int)


zeroPad : Int -> String -> String
zeroPad n =
    String.padLeft n '0'


{-| Parse a `Day` from an `Int` value.

TODO: This should accept a `Format` value.

-}
parse : Parser Day
parse =
    Parser.int
        |> Parser.andThen
            (\int ->
                case fromInt int of
                    Nothing ->
                        Parser.problem (String.fromInt int ++ " is not a valid day")

                    Just day ->
                        Parser.succeed day
            )


{-| Get the last day of the given `Year` and `Month`.
-}
lastDayOf : Year -> Month -> Day
lastDayOf year month =
    case month of
        Time.Jan ->
            Day 31

        Time.Feb ->
            if Year.isLeapYear year then
                Day 29

            else
                Day 28

        Time.Mar ->
            Day 31

        Time.Apr ->
            Day 30

        Time.May ->
            Day 31

        Time.Jun ->
            Day 30

        Time.Jul ->
            Day 31

        Time.Aug ->
            Day 31

        Time.Sep ->
            Day 30

        Time.Oct ->
            Day 31

        Time.Nov ->
            Day 30

        Time.Dec ->
            Day 31
