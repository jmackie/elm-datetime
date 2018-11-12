module Calendar.Month exposing
    ( Month
    , fromInt, fromPosix, parse
    , Format(..)
    , compare
    , toString, toInt
    )

{-| The month component of a date.


# Type definition

@docs Month


# Creating values

@docs fromInt, fromPosix, parse


# Formatting

@docs Format


# Operations

@docs compare


# Converting to other things

@docs toString, toInt

-}

import Parser exposing (Parser)
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

    > fromPosix Time.utc (Time.millisToPosix 42)
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
-}
compare : Month -> Month -> Order
compare lhs rhs =
    Basics.compare (toInt lhs) (toInt rhs)


{-| Ways in which a `Month` can be represented as a `String`.

  - `LongFormat` is the unabbreviated month name (capitalized).
  - `ShortFormat` is the first three letters of `LongFormat`.
  - `NumberFormat` is a string integer.
  - `PaddedNumberFormat` is a string integer zero-padded to lengh `n`.

See `toString` examples.

-}
type Format
    = LongFormat
    | ShortFormat
    | NumberFormat
    | PaddedNumberFormat Int


{-| Convert a `Month` to a `String` with the given format.

    > toString ShortFormat Time.Feb
    "Feb" : String

    > toString LongFormat Time.Sep
    "September" : String

    > toString NumberFormat Time.Jan
    "1" : String

    > toString (PaddedNumberFormat 2) Time.Jan
    "01" : String

    > toString (PaddedNumberFormat 2) Time.Dec
    "12" : String

String formats are capitalized by default, you might want to lower them.

-}
toString : Format -> Month -> String
toString format =
    case format of
        LongFormat ->
            toStringLongFormat

        ShortFormat ->
            toStringShortFormat

        NumberFormat ->
            toStringNumberFormat

        PaddedNumberFormat n ->
            toStringPaddedNumberFormat n


toStringLongFormat : Month -> String
toStringLongFormat month =
    case month of
        Time.Jan ->
            "January"

        Time.Feb ->
            "February"

        Time.Mar ->
            "March"

        Time.Apr ->
            "April"

        Time.May ->
            "May"

        Time.Jun ->
            "June"

        Time.Jul ->
            "July"

        Time.Aug ->
            "August"

        Time.Sep ->
            "September"

        Time.Oct ->
            "October"

        Time.Nov ->
            "November"

        Time.Dec ->
            "December"


toStringShortFormat : Month -> String
toStringShortFormat =
    String.left 3 << toStringLongFormat


toStringNumberFormat : Month -> String
toStringNumberFormat =
    String.fromInt << toInt


toStringPaddedNumberFormat : Int -> Month -> String
toStringPaddedNumberFormat n =
    String.pad n '0' << toStringNumberFormat


{-| Parse a `Month` according to the given format.

    > Parser.run (parse LongFormat) "January" |> Result.toMaybe
    Just Jan : Maybe Month

    > Parser.run (parse LongFormat) "March" |> Result.toMaybe
    Just Mar : Maybe Month

    > Parser.run (parse ShortFormat) "Jun" |> Result.toMaybe
    Just Jun : Maybe Month

    > Parser.run (parse NumberFormat) "4" |> Result.toMaybe
    Just Apr : Maybe Month

    > Parser.run (parse (PaddedNumberFormat 2)) "04" |> Result.toMaybe
    Just Apr : Maybe Month

-}
parse : Format -> Parser Month
parse format =
    case format of
        LongFormat ->
            parseLongFormat

        ShortFormat ->
            parseShortFormat

        NumberFormat ->
            parseNumberFormat

        PaddedNumberFormat n ->
            parsePaddedNumberFormat n


parseLongFormat : Parser Month
parseLongFormat =
    Parser.oneOf
        (List.map (parseMonthWith (toString LongFormat)) allTheMonths)


parseShortFormat : Parser Month
parseShortFormat =
    Parser.oneOf
        (List.map (parseMonthWith (toString ShortFormat)) allTheMonths)


parseNumberFormat : Parser Month
parseNumberFormat =
    Parser.oneOf
        (List.map (parseMonthWith (toString NumberFormat)) allTheMonths)


parsePaddedNumberFormat : Int -> Parser Month
parsePaddedNumberFormat n =
    Parser.oneOf
        (List.map (parseMonthWith (toString (PaddedNumberFormat n))) allTheMonths)


parseMonthWith : (Month -> String) -> Month -> Parser Month
parseMonthWith f month =
    parseToken (f month) month


parseToken : String -> a -> Parser a
parseToken want a =
    Parser.token want |> Parser.andThen (\_ -> Parser.succeed a)


allTheMonths : List Month
allTheMonths =
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
