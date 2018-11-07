module Calendar exposing
    ( Date
    , Format(..), isoFormat
    , fromYearMonthDay, fromRawYearMonthDay
    , toString
    , year, month, day
    )

{-| A full Gregorian Date.


# Definition

@docs Date


# String representation

@docs Format, isoFormat


# Constructing dates

@docs fromYearMonthDay, fromRawYearMonthDay


# String conversion

@docs toString


# Accessors

Provided for convenience.

@docs year, month, day

-}

import Calendar.Day as Day exposing (Day)
import Calendar.Month as Month exposing (Month)
import Calendar.Year as Year exposing (Year)
import Parser exposing (Parser)
import Task exposing (Task)


{-| A `Date` is composed of a `Year`, `Month`, and `Day`.
-}
type Date
    = Date { year : Year, month : Month, day : Day }


{-| Extract the `Year` part of a `Date`.
-}
year : Date -> Year
year (Date date) =
    date.year


{-| Extract the `Year` part of a `Date`.
-}
month : Date -> Month
month (Date date) =
    date.month


{-| Extract the `Year` part of a `Date`.
-}
day : Date -> Day
day (Date date) =
    date.day


{-| Construct a `Date` from its constituent parts.

Returns `Nothing` if the combination would form an invalid date.

-}
fromYearMonthDay : Year -> Month -> Day -> Maybe Date
fromYearMonthDay y m d =
    case Day.compare d (Day.lastDayOf y m) of
        GT ->
            Nothing

        _ ->
            Just (Date { year = y, month = m, day = d })


{-| Construct a `Date` from its raw constituent parts.

Returns `Nothing` if any parts or their combination would form an invalid date.

-}
fromRawYearMonthDay : Int -> Int -> Int -> Maybe Date
fromRawYearMonthDay rawYear rawMonth rawDay =
    Year.fromInt rawYear
        |> Maybe.andThen
            (\y ->
                Month.fromInt rawMonth
                    |> Maybe.andThen
                        (\m ->
                            Day.fromInt rawDay
                                |> Maybe.andThen (fromYearMonthDay y m)
                        )
            )


{-| Ways in which a `Date` can be represented as a `String`.
-}
type Format
    = YearFormat Year.Format Format
    | MonthFormat Month.Format Format
    | DayFormat Day.Format Format
    | Placeholder String Format
    | End


{-| "YYYY-MM-DD"
-}
isoFormat : Format
isoFormat =
    YearFormat Year.FourDigitsFormat
        (Placeholder "-"
            (MonthFormat Month.TwoDigitsFormat
                (Placeholder "-"
                    (DayFormat Day.TwoDigitsFormat End)
                )
            )
        )


{-| Convert a `Date` to a `String` with the given formats.
-}
toString : Format -> Date -> String
toString format (Date date) =
    case format of
        YearFormat yearFormat nextFormat ->
            Year.toString yearFormat date.year ++ toString nextFormat (Date date)

        MonthFormat monthFormat nextFormat ->
            Month.toString monthFormat date.month ++ toString nextFormat (Date date)

        DayFormat dayFormat nextFormat ->
            Day.toString dayFormat date.day ++ toString nextFormat (Date date)

        Placeholder string nextFormat ->
            string ++ toString nextFormat (Date date)

        End ->
            ""
