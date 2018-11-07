module Calendar.Day exposing
    ( Day, one, lastDayOf
    , Format(..)
    , compare
    , increment, add
    , fromInt, toInt
    , toString, parse
    )

{-| The day component of a date.


# Definition

@docs Day, one, lastDayOf


# String representation

@docs Format


# Comparison

@docs compare


# Arithmetic

@docs increment, add


# Integer conversion

@docs fromInt, toInt


# String conversion

@docs toString, parse


# TODO

`Day` -> `WeekDay` ?

-}

import Calendar.Month as Month exposing (Month)
import Calendar.Year as Year exposing (Year)
import Parser exposing (Parser)


{-| The day component of a date.

This is represented internally as an integer with a lower bound of 1 and
an upper bound of 31 (inclusive).

-}
type Day
    = Day Int


{-| Attempt to construct a `Day` from an `Int`.
-}
fromInt : Int -> Maybe Day
fromInt int =
    if isValid int then
        Just (Day int)

    else
        Nothing


{-| Internal logic for whether an `Int` is a valid day.
-}
isValid : Int -> Bool
isValid int =
    int >= 1 && int <= 31


{-| Convert a `Day` to an integer.
-}
toInt : Day -> Int
toInt (Day int) =
    int


{-| Add two days together. Returns `Nothing` if the resulting `Day` would be invalid.
-}
add : Day -> Day -> Maybe Day
add (Day lhs) (Day rhs) =
    fromInt (lhs + rhs)


{-| `Day` one.
-}
one : Day
one =
    Day 1


{-| Increment a `Day`. Returns `Nothing` if the resulting `Day` would be invalid.
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
-}
type Format
    = TwoDigitsFormat


{-| Convert a `Day` to a `String` with the given format.
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
        Month.January ->
            Day 31

        Month.February ->
            if Year.isLeapYear year then
                Day 29

            else
                Day 28

        Month.March ->
            Day 31

        Month.April ->
            Day 30

        Month.May ->
            Day 31

        Month.June ->
            Day 30

        Month.July ->
            Day 31

        Month.August ->
            Day 31

        Month.September ->
            Day 30

        Month.October ->
            Day 31

        Month.November ->
            Day 30

        Month.December ->
            Day 31
