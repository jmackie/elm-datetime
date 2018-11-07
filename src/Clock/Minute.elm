module Clock.Minute exposing
    ( Minute, zero
    , Format(..)
    , compare
    , increment, add
    , fromInt, toInt
    , toString, parse
    )

{-| The day component of a date.


# Definition

@docs Minute, zero


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

-}

import Parser exposing (Parser)


{-| The minute component of a time value.

This is represented internally as an integer restricted to the range 0 to 23, inclusive.

-}
type Minute
    = Minute Int


isValid : Int -> Bool
isValid int =
    int >= 0 && int <= 23


{-| Attempt to construct an `Minute` from an `Int`.
-}
fromInt : Int -> Maybe Minute
fromInt int =
    if isValid int then
        Just (Minute int)

    else
        Nothing


{-| Convert an `Minute` to an `Int`.
-}
toInt : Minute -> Int
toInt (Minute int) =
    int


{-| Add two hours together. Returns `Nothing` if the result would be invalid.
-}
add : Minute -> Minute -> Maybe Minute
add (Minute lhs) (Minute rhs) =
    fromInt (lhs + rhs)


{-| Zero hours.
-}
zero : Minute
zero =
    Minute 0


{-| Increment an `Minute`. Returns `Nothing` if the result would be invalid.
-}
increment : Minute -> Maybe Minute
increment (Minute int) =
    fromInt (int + 1)


{-| Compare two `Minute` values.
-}
compare : Minute -> Minute -> Order
compare lhs rhs =
    Basics.compare (toInt lhs) (toInt rhs)


{-| Ways in which an `Minute` can be represented as a `String`.
-}
type Format
    = TwoDigitsFormat


{-| Convert an `Minute` to a `String` with the given format.
-}
toString : Format -> Minute -> String
toString format =
    case format of
        TwoDigitsFormat ->
            toStringTwoDigitsFormat


toStringTwoDigitsFormat : Minute -> String
toStringTwoDigitsFormat (Minute int) =
    zeroPad 2 (String.fromInt int)


zeroPad : Int -> String -> String
zeroPad n =
    String.padLeft n '0'


{-| Parse an `Minute` from an `Int` value.
-}
parse : Parser Minute
parse =
    Parser.int
        |> Parser.andThen
            (\int ->
                case fromInt int of
                    Nothing ->
                        Parser.problem (String.fromInt int ++ " is not a valid minute")

                    Just minute ->
                        Parser.succeed minute
            )
