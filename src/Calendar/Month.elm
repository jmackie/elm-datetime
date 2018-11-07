module Calendar.Month exposing
    ( Month(..)
    , Format(..)
    , compare
    , fromInt, toInt
    , toString, parse
    )

{-| Gregorian month type.


# Definition

@docs Month


# String representation

@docs Format


# Comparison

@docs compare


# Integer conversion

@docs fromInt, toInt


# String conversion

@docs toString, parse

-}

import Parser exposing (Parser)


{-| A Gregorian month (January through to December).
-}
type Month
    = January
    | February
    | March
    | April
    | May
    | June
    | July
    | August
    | September
    | October
    | November
    | December


{-| An enumeration of all the months, not exposed.
-}
allTheMonths : List Month
allTheMonths =
    [ January
    , February
    , March
    , April
    , May
    , June
    , July
    , August
    , September
    , October
    , November
    , December
    ]


{-| Attempt to construct a `Month` from an `Int`.

    fromInt 1 == Just January

    fromInt 42 == Nothing

-}
fromInt : Int -> Maybe Month
fromInt int =
    case int of
        1 ->
            Just January

        2 ->
            Just February

        3 ->
            Just March

        4 ->
            Just April

        5 ->
            Just May

        6 ->
            Just June

        7 ->
            Just July

        8 ->
            Just August

        9 ->
            Just September

        10 ->
            Just October

        11 ->
            Just November

        12 ->
            Just December

        _ ->
            Nothing


{-| Convert a `Month` to an `Int`.

    toInt January == 1

    toInt December == 12

-}
toInt : Month -> Int
toInt month =
    case month of
        January ->
            1

        February ->
            2

        March ->
            3

        April ->
            4

        May ->
            5

        June ->
            6

        July ->
            7

        August ->
            8

        September ->
            9

        October ->
            10

        November ->
            11

        December ->
            12


{-| Compare two `Month` values.
-}
compare : Month -> Month -> Order
compare lhs rhs =
    Basics.compare (toInt lhs) (toInt rhs)


{-| Ways in which a `Month` can be represented as a `String`.

    toString FullFormat January == "January"

    toString ShortFormat January == "Jan"

    toString TwoDigits January == "1"

Note `ShortFormat` is the first three letters of `FullFormat`.

-}
type Format
    = FullFormat
    | ShortFormat
    | TwoDigitsFormat


{-| Convert a `Month` to a `String` with the given format.

    toString ShortFormat February == "Feb"

    toString ShortFormat January == "Jan"

    toString TwoDigits January == "1"

-}
toString : Format -> Month -> String
toString format =
    case format of
        FullFormat ->
            toStringFullFormat

        ShortFormat ->
            toStringShortFormat

        TwoDigitsFormat ->
            toStringTwoDigitsFormat


toStringFullFormat : Month -> String
toStringFullFormat month =
    case month of
        January ->
            "January"

        February ->
            "February"

        March ->
            "March"

        April ->
            "April"

        May ->
            "May"

        June ->
            "June"

        July ->
            "July"

        August ->
            "August"

        September ->
            "September"

        October ->
            "October"

        November ->
            "November"

        December ->
            "December"


toStringShortFormat : Month -> String
toStringShortFormat =
    String.left 3 << toStringFullFormat


toStringTwoDigitsFormat : Month -> String
toStringTwoDigitsFormat month =
    let
        monthInt =
            toInt month
    in
    if monthInt < 10 then
        String.cons '0' (String.fromInt monthInt)

    else
        String.fromInt monthInt


{-| Parse a `Month` according to the given format.
-}
parse : Format -> Parser Month
parse format =
    case format of
        FullFormat ->
            parseFullFormat

        ShortFormat ->
            parseShortFormat

        TwoDigitsFormat ->
            parseTwoDigitsFormat


parseFullFormat : Parser Month
parseFullFormat =
    Parser.oneOf (List.map (parseMonthWith (toString FullFormat)) allTheMonths)


parseShortFormat : Parser Month
parseShortFormat =
    Parser.oneOf (List.map (parseMonthWith (toString ShortFormat)) allTheMonths)


parseTwoDigitsFormat : Parser Month
parseTwoDigitsFormat =
    Parser.oneOf (List.map (parseMonthWith (toString TwoDigitsFormat)) allTheMonths)


parseMonthWith : (Month -> String) -> Month -> Parser Month
parseMonthWith f month =
    parseToken (f month) month


{-| TODO: Should this be case insensitive?
-}
parseToken : String -> a -> Parser a
parseToken want a =
    Parser.token want |> Parser.andThen (\_ -> Parser.succeed a)
