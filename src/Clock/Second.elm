module Clock.Second exposing
    ( Second, zero
    , Format(..)
    , compare
    , add
    , fromFloat, toFloat
    , toString, parse
    )

{-| The seconds component of a time value.


# Definition

@docs Second, zero


# String representation

@docs Format


# Comparison

@docs compare


# Arithmetic

@docs add


# Float conversion

@docs fromFloat, toFloat


# String conversion

@docs toString, parse

-}

import Parser exposing (Parser)


{-| The second component of a time value.
-}
type Second
    = Second Float


isValid : Float -> Bool
isValid int =
    int >= 0 && int < 60


{-| Attempt to construct a `Second` from a `Float`.
-}
fromFloat : Float -> Maybe Second
fromFloat float =
    if isValid float then
        Just (Second float)

    else
        Nothing


{-| Convert a `Second` to a `Float`.
-}
toFloat : Second -> Float
toFloat (Second float) =
    float


{-| Add two `Second` values together. Returns `Nothing` if the result would be invalid.
-}
add : Second -> Second -> Maybe Second
add (Second lhs) (Second rhs) =
    fromFloat (lhs + rhs)


{-| Zero seconds.
-}
zero : Second
zero =
    Second 0.0


{-| Compare two `Second` values.
-}
compare : Second -> Second -> Order
compare lhs rhs =
    Basics.compare (toFloat lhs) (toFloat rhs)


{-| Ways in which an `Second` can be represented as a `String`.
-}
type Format
    = TwoDigitsFormat


{-| Convert a `Second` value to a `String` with the given format.
-}
toString : Format -> Second -> String
toString format =
    case format of
        TwoDigitsFormat ->
            toStringTwoDigitsFormat


toStringTwoDigitsFormat : Second -> String
toStringTwoDigitsFormat (Second float) =
    zeroPad 2 (String.fromFloat float)


zeroPad : Int -> String -> String
zeroPad n =
    String.padLeft n '0'


{-| Parse an `Second` from an `Int` value.
-}
parse : Parser Second
parse =
    Parser.float
        |> Parser.andThen
            (\float ->
                case fromFloat float of
                    Nothing ->
                        Parser.problem (String.fromFloat float ++ " is not a valid second value")

                    Just second ->
                        Parser.succeed second
            )
