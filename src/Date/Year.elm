module Date.Year
    exposing
        ( Format(..)
        , Year
        , add
        , compare
        , fromInt
        , increment
        , isLeapYear
        , parse
        , toInt
        , toString
        , zero
        )

{-| The year component of a date.


# Definition

@docs Year, zero


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


# Questions

@docs isLeapYear

-}

import Parser exposing (Parser)


{-| The year component of a date.

This is represented internally as a year with a lower bound of 0 (inclusive).

-}
type Year
    = Year Int


{-| Attempt to construct a `Year` from an `Int`.
-}
fromInt : Int -> Maybe Year
fromInt int =
    if isValid int then
        Just (Year int)

    else
        Nothing


{-| Internal logic for whether an `Int` is a valid year.
-}
isValid : Int -> Bool
isValid int =
    int > 0


{-| Add two years together.
-}
add : Year -> Year -> Year
add (Year lhs) (Year rhs) =
    Year (lhs + rhs)


{-| Convert a `Year` to an `Int`.
-}
toInt : Year -> Int
toInt (Year int) =
    int


{-| `Year` zero.
-}
zero : Year
zero =
    Year 0


{-| Increment a `Year`
-}
increment : Year -> Year
increment (Year int) =
    Year (int + 1)


{-| Compare two `Year` values.
-}
compare : Year -> Year -> Order
compare lhs rhs =
    Basics.compare (toInt lhs) (toInt rhs)


{-| Ways in which a `Year` can be represented as a `String`.
-}
type Format
    = TwoDigitsFormat
    | FourDigitsFormat


{-| Convert a `Year` to a `String` with the given format.
-}
toString : Format -> Year -> String
toString format =
    case format of
        TwoDigitsFormat ->
            toStringTwoDigitsFormat

        FourDigitsFormat ->
            toStringFourDigitsFormat


toStringTwoDigitsFormat : Year -> String
toStringTwoDigitsFormat (Year int) =
    String.fromInt (remainderBy 100 int)


toStringFourDigitsFormat : Year -> String
toStringFourDigitsFormat (Year int) =
    String.fromInt int


{-| Parse a year from an `Int` value.

Note if you expect a two digit year (for some reason?) then you should correct
it manually based on the current year.

-}
parse : Parser Year
parse =
    Parser.int
        |> Parser.andThen
            (\int ->
                case fromInt int of
                    Nothing ->
                        Parser.problem (String.fromInt int ++ " is not a valid year")

                    Just year ->
                        Parser.succeed year
            )


{-| Check whether the given year was a leap year.
-}
isLeapYear : Year -> Bool
isLeapYear (Year int) =
    (modBy 4 int == 0) && ((modBy 400 int == 0) || not (modBy 100 int == 0))
