module DateTime exposing
    ( DateTime
    , fromPosix
    )

{-| A complete datetime type.

@docs DateTime


# Creating a `DateTime`

@docs fromPosix


# Conversions

@docs toMillis, toPosix

-}

import Calendar
import Clock
import Time


{-| A `DateTime` is the combination of a (Gregorian) calendar date, a clock time, and a time zone.
-}
type alias DateTime =
    { date : Calendar.Date
    , time : Clock.Time
    , zone : Time.Zone
    }


{-| If you have a `Posix` time and a time `Zone` you can have a `DateTime` (and vice versa).
-}
fromPosix : Time.Zone -> Time.Posix -> DateTime
fromPosix zone posix =
    { date = Calendar.fromPosix zone posix
    , time = Clock.fromPosix zone posix
    , zone = zone
    }


{-| Turn a `DateTime` into a `Posix` time.
-}
toPosix : DateTime -> Time.Posix
toPosix =
    toMillis >> Time.millisToPosix


{-| Turn a `DateTime` into the number of milliseconds since 1970 January 1 at 00:00:00 UTC.

TODO

-}
toMillis : DateTime -> Int
toMillis dateTime =
    Clock.toMillis dateTime.time


oneDayInMillis : Int
oneDayInMillis =
    -- 1000 * 60 * 60 * 24
    86400000
