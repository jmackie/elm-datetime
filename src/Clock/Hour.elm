module Clock.Hour exposing
    ( Hour, zero
    , Format(..)
    , compare
    , increment, add
    , fromInt, toInt
    , toString, parse
    )

{-| The day component of a date.


# Definition

@docs Hour, zero


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

Twelve and twenty four hour formats.

-}

import Parser exposing (Parser)


{-| The hour component of a time value.

This is represented internally as an integer restricted to the range 0 to 23, inclusive.

-}
type Hour
    = Hour Int


isValid : Int -> Bool
isValid int =
    int >= 0 && int <= 23


{-| Attempt to construct an `Hour` from an `Int`.
-}
fromInt : Int -> Maybe Hour
fromInt int =
    if isValid int then
        Just (Hour int)

    else
        Nothing


{-| Convert an `Hour` to an `Int`.
-}
toInt : Hour -> Int
toInt (Hour int) =
    int


{-| Add two hours together. Returns `Nothing` if the result would be invalid.
-}
add : Hour -> Hour -> Maybe Hour
add (Hour lhs) (Hour rhs) =
    fromInt (lhs + rhs)


{-| Zero hours.
-}
zero : Hour
zero =
    Hour 0


{-| Increment an `Hour`. Returns `Nothing` if the result would be invalid.
-}
increment : Hour -> Maybe Hour
increment (Hour int) =
    fromInt (int + 1)


{-| Compare two `Hour` values.
-}
compare : Hour -> Hour -> Order
compare lhs rhs =
    Basics.compare (toInt lhs) (toInt rhs)


{-| Ways in which an `Hour` can be represented as a `String`.
-}
type Format
    = TwoDigitsFormat


{-| Convert an `Hour` to a `String` with the given format.
-}
toString : Format -> Hour -> String
toString format =
    case format of
        TwoDigitsFormat ->
            toStringTwoDigitsFormat


toStringTwoDigitsFormat : Hour -> String
toStringTwoDigitsFormat (Hour int) =
    zeroPad 2 (String.fromInt int)


zeroPad : Int -> String -> String
zeroPad n =
    String.padLeft n '0'


{-| Parse an `Hour` from an `Int` value.
-}
parse : Parser Hour
parse =
    Parser.int
        |> Parser.andThen
            (\int ->
                case fromInt int of
                    Nothing ->
                        Parser.problem (String.fromInt int ++ " is not a valid hour")

                    Just hour ->
                        Parser.succeed hour
            )
