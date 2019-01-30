module Calendar.Internal exposing
    ( Date(..)
    , DateRecord
    , Day(..)
    , Month
    , Raw
    , Year(..)
    , allTheMonths
    , compare
    , compareDay
    , compareMonth
    , compareYear
    , dayFromInt
    , dayToInt
    , decrementYear
    , fromPosix
    , fromRaw
    , getDateRange
    , getDateRange_
    , getDatesInMonth
    , getDay
    , getDayDiff
    , getFollowingMonths
    , getMonth
    , getNextDay
    , getNextMonth
    , getPrecedingMonths
    , getPreviousDay
    , getPreviousMonth
    , getYear
    , incrementYear
    , isLeapYear
    , lastDayOf
    , millisInADay
    , millisInYear
    , millisSinceEpoch
    , millisSinceStartOfTheMonth
    , millisSinceStartOfTheYear
    , monthFromInt
    , monthToInt
    , rollMonthBack
    , rollMonthForward
    , toEpochMillis
    , toPosix
    , weekdayFromDate
    , yearFromInt
    , yearToInt
    )

import Array exposing (Array)
import Common exposing (applyMaybe, comparing, foldOrders)
import Time


type Date
    = Date DateRecord


type alias DateRecord =
    { year : Year
    , month : Month
    , day : Day
    }


getYear : Date -> Year
getYear (Date { year }) =
    year


getMonth : Date -> Month
getMonth (Date { month }) =
    month


getDay : Date -> Day
getDay (Date date) =
    date.day


fromPosix : Time.Posix -> Date
fromPosix posix =
    Date
        { year = Year (Time.toYear Time.utc posix)
        , month = Time.toMonth Time.utc posix
        , day = Day (Time.toDay Time.utc posix)
        }


fromRaw : Raw -> Maybe Date
fromRaw { year, month, day } =
    (Just Tuple.pair
        |> applyMaybe (yearFromInt year)
        |> applyMaybe (monthFromInt month)
    )
        |> Maybe.andThen
            (\( y, m ) ->
                Maybe.map (Date << DateRecord y m) (dayFromInt y m day)
            )


type alias Raw =
    { year : Int
    , month : Int
    , day : Int
    }


compare : Date -> Date -> Order
compare lhs rhs =
    foldOrders
        (comparing (getYear >> yearToInt) lhs rhs)
        [ comparing (getMonth >> monthToInt) lhs rhs
        , comparing (getDay >> dayToInt) lhs rhs
        ]


compareYear : Year -> Year -> Order
compareYear =
    comparing yearToInt


compareMonth : Month -> Month -> Order
compareMonth =
    comparing monthToInt


compareDay : Day -> Day -> Order
compareDay =
    comparing dayToInt


toPosix : Date -> Time.Posix
toPosix =
    Time.millisToPosix << toEpochMillis


toEpochMillis : Date -> Int
toEpochMillis (Date { year, month, day }) =
    millisSinceEpoch year
        + millisSinceStartOfTheYear year month
        + millisSinceStartOfTheMonth day


millisSinceStartOfTheYear : Year -> Month -> Int
millisSinceStartOfTheYear year month =
    List.foldl
        (\m res ->
            res + (millisInADay * dayToInt (lastDayOf year m))
        )
        0
        (getPrecedingMonths month)


millisSinceStartOfTheMonth : Day -> Int
millisSinceStartOfTheMonth day =
    millisInADay * (dayToInt day - 1)


weekdayFromDate : Date -> Time.Weekday
weekdayFromDate date =
    Time.toWeekday Time.utc (toPosix date)


getPrecedingMonths : Month -> List Month
getPrecedingMonths month =
    Array.toList <|
        Array.slice 0 (monthToInt month - 1) allTheMonths


getFollowingMonths : Month -> List Month
getFollowingMonths month =
    Array.toList <|
        Array.slice (monthToInt month) 12 allTheMonths


getNextMonth : Date -> Date
getNextMonth (Date date) =
    let
        updatedMonth =
            rollMonthForward date.month

        updatedYear =
            case updatedMonth of
                Time.Jan ->
                    Year (yearToInt date.year + 1)

                _ ->
                    date.year

        lastDayOfUpdatedMonth =
            lastDayOf updatedYear updatedMonth

        updatedDay =
            case compareDay date.day lastDayOfUpdatedMonth of
                GT ->
                    lastDayOfUpdatedMonth

                _ ->
                    date.day
    in
    Date
        { year = updatedYear
        , month = updatedMonth
        , day = updatedDay
        }


rollMonthForward : Month -> Month
rollMonthForward month =
    case month of
        Time.Jan ->
            Time.Feb

        Time.Feb ->
            Time.Mar

        Time.Mar ->
            Time.Apr

        Time.Apr ->
            Time.May

        Time.May ->
            Time.Jun

        Time.Jun ->
            Time.Jul

        Time.Jul ->
            Time.Aug

        Time.Aug ->
            Time.Sep

        Time.Sep ->
            Time.Oct

        Time.Oct ->
            Time.Nov

        Time.Nov ->
            Time.Dec

        Time.Dec ->
            -- roll
            Time.Jan


getPreviousMonth : Date -> Date
getPreviousMonth (Date date) =
    let
        updatedMonth =
            rollMonthBack date.month

        updatedYear =
            case updatedMonth of
                Time.Dec ->
                    Year (yearToInt date.year - 1)

                _ ->
                    date.year

        lastDayOfUpdatedMonth =
            lastDayOf updatedYear updatedMonth

        updatedDay =
            case compareDay date.day lastDayOfUpdatedMonth of
                GT ->
                    lastDayOfUpdatedMonth

                _ ->
                    date.day
    in
    Date
        { year = updatedYear
        , month = updatedMonth
        , day = updatedDay
        }


rollMonthBack : Month -> Month
rollMonthBack month =
    case month of
        Time.Jan ->
            -- roll
            Time.Dec

        Time.Feb ->
            Time.Jan

        Time.Mar ->
            Time.Feb

        Time.Apr ->
            Time.Mar

        Time.May ->
            Time.Apr

        Time.Jun ->
            Time.May

        Time.Jul ->
            Time.Jun

        Time.Aug ->
            Time.Jul

        Time.Sep ->
            Time.Aug

        Time.Oct ->
            Time.Sep

        Time.Nov ->
            Time.Oct

        Time.Dec ->
            Time.Nov


incrementYear : Date -> Date
incrementYear (Date date) =
    let
        updatedYear =
            Year (yearToInt date.year + 1)

        lastDayOfUpdatedMonth =
            lastDayOf updatedYear date.month

        updatedDay =
            case compareDay date.day lastDayOfUpdatedMonth of
                GT ->
                    lastDayOfUpdatedMonth

                _ ->
                    date.day
    in
    Date
        { year = updatedYear
        , month = date.month
        , day = updatedDay
        }


decrementYear : Date -> Date
decrementYear (Date date) =
    let
        updatedYear =
            Year (yearToInt date.year - 1)

        lastDayOfUpdatedMonth =
            lastDayOf updatedYear date.month

        updatedDay =
            case compareDay date.day lastDayOfUpdatedMonth of
                GT ->
                    lastDayOfUpdatedMonth

                _ ->
                    date.day
    in
    Date
        { year = updatedYear
        , month = date.month
        , day = updatedDay
        }


getDatesInMonth : Year -> Month -> List Date
getDatesInMonth year month =
    let
        lastDayOfTheMonth =
            dayToInt (lastDayOf year month)
    in
    List.map (Date << DateRecord year month << Day)
        (List.range 1 lastDayOfTheMonth)


getNextDay : Date -> Date
getNextDay date =
    let
        millis =
            Time.posixToMillis (toPosix date) + millisInADay

        newDate =
            fromPosix (Time.millisToPosix millis)
    in
    newDate


getPreviousDay : Date -> Date
getPreviousDay date =
    let
        millis =
            Time.posixToMillis (toPosix date) - millisInADay

        newDate =
            fromPosix (Time.millisToPosix millis)
    in
    newDate


getDateRange : Date -> Date -> List Date
getDateRange startDate endDate =
    let
        ( startPosix, endPosix ) =
            ( toPosix startDate
            , toPosix endDate
            )

        posixDiff =
            Time.posixToMillis endPosix - Time.posixToMillis startPosix

        daysDiff =
            posixDiff // 1000 // 60 // 60 // 24
    in
    if daysDiff > 0 then
        getDateRange_ daysDiff startDate []

    else
        getDateRange_ (abs daysDiff) endDate []


getDateRange_ : Int -> Date -> List Date -> List Date
getDateRange_ daysCount prevDate res =
    let
        updatedRes =
            res ++ [ prevDate ]
    in
    if daysCount > 0 then
        let
            ( updatedDaysCount, updatedPrevDate ) =
                ( daysCount - 1
                , getNextDay prevDate
                )
        in
        getDateRange_ updatedDaysCount updatedPrevDate updatedRes

    else
        updatedRes


getDayDiff : Date -> Date -> Int
getDayDiff startDate endDate =
    let
        ( startPosix, endPosix ) =
            ( toPosix startDate
            , toPosix endDate
            )

        posixDiff =
            Time.posixToMillis endPosix - Time.posixToMillis startPosix
    in
    posixDiff // 1000 // 60 // 60 // 24


type Year
    = Year Int


yearToInt : Year -> Int
yearToInt (Year year) =
    year


yearFromInt : Int -> Maybe Year
yearFromInt year =
    -- NOTE: no upper bound on Year!
    if year > 0 then
        Just (Year year)

    else
        Nothing


type alias Month =
    Time.Month


monthToInt : Month -> Int
monthToInt month =
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


monthFromInt : Int -> Maybe Month
monthFromInt int =
    Array.get (int - 1) allTheMonths


type Day
    = Day Int


dayToInt : Day -> Int
dayToInt (Day day) =
    day


dayFromInt : Year -> Month -> Int -> Maybe Day
dayFromInt year month day =
    let
        maxValidDay =
            dayToInt (lastDayOf year month)
    in
    if day > 0 && isNotGreaterThan maxValidDay day then
        Just (Day day)

    else
        Nothing


isLeapYear : Year -> Bool
isLeapYear (Year int) =
    (modBy 4 int == 0) && ((modBy 400 int == 0) || not (modBy 100 int == 0))


lastDayOf : Year -> Month -> Day
lastDayOf year month =
    case month of
        Time.Jan ->
            Day 31

        Time.Feb ->
            if isLeapYear year then
                Day 29

            else
                Day 28

        Time.Mar ->
            Day 31

        Time.Apr ->
            Day 30

        Time.May ->
            Day 31

        Time.Jun ->
            Day 30

        Time.Jul ->
            Day 31

        Time.Aug ->
            Day 31

        Time.Sep ->
            Day 30

        Time.Oct ->
            Day 31

        Time.Nov ->
            Day 30

        Time.Dec ->
            Day 31


millisSinceEpoch : Year -> Int
millisSinceEpoch (Year year) =
    List.foldl
        (\y result ->
            let
                yearMillis =
                    Maybe.withDefault 0 <|
                        Maybe.map millisInYear (yearFromInt y)
            in
            result + yearMillis
        )
        0
        -- (year - 1) because we want the milliseconds
        -- in the start of the target year in order to add
        -- the months + days + hours + minues + secs if we want to.
        (List.range 1970 (year - 1))


millisInADay : Int
millisInADay =
    1000 * 60 * 60 * 24


millisInYear : Year -> Int
millisInYear year =
    if isLeapYear year then
        1000 * 60 * 60 * 24 * 366

    else
        1000 * 60 * 60 * 24 * 365


allTheMonths : Array Month
allTheMonths =
    Array.fromList
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


isNotGreaterThan : comparable -> comparable -> Bool
isNotGreaterThan a b =
    Basics.compare b a /= GT
