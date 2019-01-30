module DateTime exposing
    ( DateTime
    , fromPosix, toPosix
    )

{-|

@docs DateTime


# Converting to and from `Posix`

@docs fromPosix, toPosix

-}

import Calendar
import Calendar.Internal
import Clock
import Clock.Internal
import Time


{-| A `DateTime`, as the name implies, is the combination of a calendar date and clock time.

It has no notion of time zone and is isomporphic to `Posix` time.

-}
type alias DateTime =
    { date : Calendar.Date
    , time : Clock.Time
    }



-- POSIX CONVERSION --


{-| Convert a `Posix` time to a `DateTime`.
-}
fromPosix : Time.Posix -> DateTime
fromPosix posix =
    { date = Calendar.fromPosix posix
    , time = Clock.fromPosix posix
    }


{-| Convert a `DateTime` to a `Posix` time.
-}
toPosix : DateTime -> Time.Posix
toPosix { date, time } =
    Time.millisToPosix
        (Calendar.Internal.toEpochMillis date + Clock.Internal.toMillis time)
