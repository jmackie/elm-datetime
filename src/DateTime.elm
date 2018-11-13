module DateTime exposing
    ( DateTime
    , fromPosix, fromUtcDateAndTime
    , date, year, month, day, time, zone, hour, minute, second, millisecond
    , toPosix
    )

{-| A complete datetime type.

@docs DateTime


# Creating a `DateTime`

@docs fromPosix, fromUtcDateAndTime


# Accessors

@docs date, year, month, day, time, zone, hour, minute, second, millisecond


# Conversions

@docs toPosix

-}

import Calendar
import Calendar.Day as Day exposing (Day)
import Calendar.Month as Month exposing (Month)
import Calendar.Year as Year exposing (Year)
import Clock
import Clock.Hour exposing (Hour)
import Clock.Minute exposing (Minute)
import Clock.Second exposing (Second)
import Time


{-| An instant in time, composed of a calendar date, clock time and time zone.
-}
type DateTime
    = DateTime InternalDateTime


type alias InternalDateTime =
    { date : Calendar.Date
    , time : Clock.Time
    , zone : Time.Zone
    , posix : Time.Posix
    }


{-| Create a `DateTime` from a time zone and posix time.

    > fromPosix Time.utc (Time.millisToPosix 0) |> year
    Year 1970 : Year

-}
fromPosix : Time.Zone -> Time.Posix -> DateTime
fromPosix timeZone timePosix =
    DateTime
        { date = Calendar.fromPosix timeZone timePosix
        , time = Clock.fromPosix timeZone timePosix
        , zone = timeZone
        , posix = timePosix
        }


{-| Create a `DateTime` from a UTC date and time.
-}
fromUtcDateAndTime : Calendar.Date -> Clock.Time -> DateTime
fromUtcDateAndTime calendarDate clockTime =
    let
        posix : Time.Posix
        posix =
            Time.millisToPosix <|
                daysToMillis (daysSinceEpoch calendarDate)
                    + Clock.toMillis clockTime
    in
    DateTime
        { date = calendarDate
        , time = clockTime
        , posix = posix
        , zone = Time.utc
        }


daysSinceEpoch : Calendar.Date -> Int
daysSinceEpoch calendarDate =
    let
        yearDays : Int
        yearDays =
            List.range 1970 (Year.toInt (Calendar.year calendarDate))
                |> List.reverse
                |> List.drop 1
                |> List.map daysInYear
                |> List.sum

        monthDays : Int
        monthDays =
            List.foldl
                (\m accum ->
                    case Month.compare m (Calendar.month calendarDate) of
                        LT ->
                            accum + daysInMonth (Calendar.year calendarDate) m

                        EQ ->
                            accum

                        GT ->
                            accum
                )
                0
                Month.months
    in
    yearDays + monthDays + Day.toInt (Calendar.day calendarDate) - 1


daysInYear : Int -> Int
daysInYear y =
    if isLeapYear y then
        366

    else
        365


isLeapYear : Int -> Bool
isLeapYear y =
    -- NOTE: copied from Calendar.Year to work with Int
    (modBy 4 y == 0) && ((modBy 400 y == 0) || not (modBy 100 y == 0))


daysInMonth : Year -> Month -> Int
daysInMonth y m =
    Day.lastDayOf y m |> Day.toInt


daysToMillis : Int -> Int
daysToMillis days =
    -- 1000 * 60 * 60 * 24
    days * 86400000


{-| Extract the calendar date from a `DateTime`.

    > date (fromPosix Time.utc (Time.millisToPosix 0))
    Date { day = Day 1, month = Jan, year = Year 1970 } : Calendar.Date

-}
date : DateTime -> Calendar.Date
date (DateTime dateTime) =
    dateTime.date


{-| Extract the calendar year from a `DateTime`.

    > year (fromPosix Time.utc (Time.millisToPosix 0))
    Year 1970 : Year

-}
year : DateTime -> Year
year =
    date >> Calendar.year


{-| Extract the calendar month from a `DateTime`.

    > month (fromPosix Time.utc (Time.millisToPosix 0))
    Jan : Month

-}
month : DateTime -> Month
month =
    date >> Calendar.month


{-| Extract the calendar day from a `DateTime`.

    > day (fromPosix Time.utc (Time.millisToPosix 0))
    Day 1 : Day

-}
day : DateTime -> Day
day =
    date >> Calendar.day


{-| Extract the clock time from a `DateTime`.

    > time (fromPosix Time.utc (Time.millisToPosix 0))
    { hour = Hour 0, millisecond = 0, minute = Minute 0, second = Second 0 } : Clock.Time

-}
time : DateTime -> Clock.Time
time (DateTime dateTime) =
    dateTime.time


{-| Extract the clock hour from a `DateTime`.

    > hour (fromPosix Time.utc (Time.millisToPosix 0))
    Hour 0 : Hour

-}
hour : DateTime -> Hour
hour =
    time >> .hour


{-| Extract the clock minute from a `DateTime`.

    > minute (fromPosix Time.utc (Time.millisToPosix 0))
    Minute 0 : Minute

-}
minute : DateTime -> Minute
minute =
    time >> .minute


{-| Extract the clock second from a `DateTime`.

    > second (fromPosix Time.utc (Time.millisToPosix 0))
    Second 0 : Second

-}
second : DateTime -> Second
second =
    time >> .second


{-| Extract the clock second from a `DateTime`.

    > millisecond (fromPosix Time.utc (Time.millisToPosix 0))
    0 : Int

-}
millisecond : DateTime -> Int
millisecond =
    time >> .millisecond


{-| Extract the time zone from a `DateTime`.

    > zone (fromPosix Time.utc (Time.millisToPosix 0))
    Zone 0 [] : Time.Zone

-}
zone : DateTime -> Time.Zone
zone (DateTime dateTime) =
    dateTime.zone


{-| Get back a posix time.

    > toPosix (fromPosix Time.utc (Time.millisToPosix 0))
    Posix 0 : Time.Posix

-}
toPosix : DateTime -> Time.Posix
toPosix (DateTime dateTime) =
    dateTime.posix
