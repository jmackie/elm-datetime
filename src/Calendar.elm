module Calendar exposing
    ( Date
    , fromPosix, fromRaw
    , year, month, day
    , compare
    )

{-|

@docs Date


# Constructing a `Date`

@docs fromPosix, fromRaw


# Deconstructing a `Date`

@docs year, month, day


# Operations over `Date`

@docs compare

-}

import Calendar.Internal as Internal
import Time


{-| A full Gregorian calendar date
-}
type alias Date =
    Internal.Date



-- CONSTRUCTION --


{-| Get the calendar date out of a `Posix` time.
-}
fromPosix : Time.Posix -> Date
fromPosix =
    Internal.fromPosix


{-| Attempt to construct a calendar date from its integer parts.
-}
fromRaw : { year : Int, month : Int, day : Int } -> Maybe Date
fromRaw =
    Internal.fromRaw



-- DECONSTRUCTION --


{-| Get the year part out of a calendar date.
-}
year : Date -> Int
year =
    Internal.getYear >> Internal.yearToInt


{-| Get the month part out of a calendar date.
-}
month : Date -> Time.Month
month =
    Internal.getMonth


{-| Get the day part out of a calendar date.
-}
day : Date -> Int
day =
    Internal.getDay >> Internal.dayToInt



-- OPERATIONS --


{-| Compare two clock times.
-}
compare : Date -> Date -> Order
compare =
    Internal.compare
