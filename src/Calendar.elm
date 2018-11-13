module Calendar exposing
    ( Date
    , fromYearMonthDay, fromRawYearMonthDay, fromPosix
    , year, month, day
    )

{-| A calendar date.


# Type definition

@docs Date


# Creating values

@docs fromYearMonthDay, fromRawYearMonthDay, fromPosix


# Accessors

@docs year, month, day

-}

import Calendar.Day as Day exposing (Day)
import Calendar.Month as Month exposing (Month)
import Calendar.Year as Year exposing (Year)
import Time


{-| A full (Gregorian) calendar date.
-}
type Date
    = Date InternalDate


type alias InternalDate =
    { year : Year
    , month : Month
    , day : Day
    }


{-| Extract the `Year` part of a `Date`.

    > year (fromPosix Time.utc (Time.millisToPosix 0))
    Year 1970 : Year

-}
year : Date -> Year
year (Date date) =
    date.year


{-| Extract the `Year` part of a `Date`.

    > month (fromPosix Time.utc (Time.millisToPosix 0))
    Jan : Month

-}
month : Date -> Month
month (Date date) =
    date.month


{-| Extract the `Year` part of a `Date`.

    > day (fromPosix Time.utc (Time.millisToPosix 0))
    Day 1 : Day

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


{-| Construct a `Date` from its (raw) constituent parts.

Returns `Nothing` if any parts or their combination would form an invalid date.

    > fromRawYearMonthDay 2018 12 11
    Just (Date { day = Day 11, month = Dec, year = Year 2018 }) : Maybe Date

    > fromRawYearMonthDay 2018 2 31
    Nothing : Maybe Date

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


{-| Get a `Date` from a time zone and posix time.

    > fromPosix Time.utc (Time.millisToPosix 0)
    Date { day = Day 1, month = Jan, year = Year 1970 } : Date

-}
fromPosix : Time.Zone -> Time.Posix -> Date
fromPosix zone posix =
    Date
        { year = Year.fromPosix zone posix
        , month = Month.fromPosix zone posix
        , day = Day.fromPosix zone posix
        }
