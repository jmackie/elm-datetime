module Clock exposing
    ( Time
    , fromPosix, fromRaw, zero
    , hour, minute, second, millis
    , compare
    , incrementHour, incrementMinute, incrementSecond, incrementMillis
    , decrementHour, decrementMinute, decrementSecond, decrementMillis
    )

{-| Clock time.

@docs Time


# Constructing a `Time`

@docs fromPosix, fromRaw, zero


# Deconstructing a `Time`

@docs hour, minute, second, millis


# Operations over `Time`

@docs compare


# Incrementing a `Time`

@docs incrementHour, incrementMinute, incrementSecond, incrementMillis


# Decrementing a `Time`

@docs decrementHour, decrementMinute, decrementSecond, decrementMillis

-}

import Clock.Internal as Internal
import Time


{-| A clock time.
-}
type alias Time =
    Internal.Time



-- CONSTRUCTION --


{-| Get the clock time out of a `Posix` time.
-}
fromPosix : Time.Posix -> Time
fromPosix =
    Internal.fromPosix


{-| Attempt to construct a clock time from its integer parts.
-}
fromRaw : { hour : Int, minute : Int, second : Int, millis : Int } -> Maybe Time
fromRaw =
    Internal.fromRaw


{-| Zero time.
-}
zero : Time
zero =
    Internal.zero



-- DECONSTRUCTION --


{-| Get the hour part out of a clock time.
-}
hour : Time -> Int
hour =
    Internal.getHour >> Internal.hourToInt


{-| Get the minute part out of a clock time.
-}
minute : Time -> Int
minute =
    Internal.getMinute >> Internal.minuteToInt


{-| Get the second part out of a clock time.
-}
second : Time -> Int
second =
    Internal.getSecond >> Internal.secondToInt


{-| Get the second part out of a clock time.
-}
millis : Time -> Int
millis =
    Internal.getMillis >> Internal.millisToInt



-- OPERATIONS --


{-| Compare two clock times.
-}
compare : Time -> Time -> Order
compare =
    Internal.compare



-- INCREMENTING --


{-| Increment the hour part of a clock time.

Returns the incremented time and a `Bool` indicating whether the clock has rolled
over 24 hours.

TODO: example to demonstrate the meaning of the return

-}
incrementHour : Time -> ( Time, Bool )
incrementHour =
    Internal.incrementHour


{-| Increment the minute part of a clock time.

Returns the incremented time and a `Bool` indicating whether the clock has rolled
over 24 hours.

TODO: example to demonstrate the meaning of the return

-}
incrementMinute : Time -> ( Time, Bool )
incrementMinute =
    Internal.incrementMinute


{-| Increment the second part of a clock time.

Returns the incremented time and a `Bool` indicating whether the clock has rolled
over 24 hours.

TODO: example to demonstrate the meaning of the return

-}
incrementSecond : Time -> ( Time, Bool )
incrementSecond =
    Internal.incrementSecond


{-| Increment the millisecond part of a clock time.

Returns the incremented time and a `Bool` indicating whether the clock has rolled
over 24 hours.

TODO: example to demonstrate the meaning of the return

-}
incrementMillis : Time -> ( Time, Bool )
incrementMillis =
    Internal.incrementMillis



-- DECREMENTING --


{-| Decrement the hour part of a clock time.

Returns the incremented time and a `Bool` indicating whether the clock has rolled
over 24 hours.

TODO: example to demonstrate the meaning of the return

-}
decrementHour : Time -> ( Time, Bool )
decrementHour =
    Internal.decrementHour


{-| Decrement the minute part of a clock time.

Returns the decremented time and a `Bool` indicating whether the clock has rolled
over 24 hours.

TODO: example to demonstrate the meaning of the return

-}
decrementMinute : Time -> ( Time, Bool )
decrementMinute =
    Internal.decrementMinute


{-| Decrement the second part of a clock time.

Returns the decremented time and a `Bool` indicating whether the clock has rolled
over 24 hours.

TODO: example to demonstrate the meaning of the return

-}
decrementSecond : Time -> ( Time, Bool )
decrementSecond =
    Internal.decrementSecond


{-| Decrement the millisecond part of a clock time.

Returns the decremented time and a `Bool` indicating whether the clock has rolled
over 24 hours.

TODO: example to demonstrate the meaning of the return

-}
decrementMillis : Time -> ( Time, Bool )
decrementMillis =
    Internal.decrementMillis
