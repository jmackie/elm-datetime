module Time.Milliseconds exposing (Milliseconds)

{-| Useful constants for converting from millisecond times.
-}


type alias Milliseconds =
    Int


oneSecond : Milliseconds
oneSecond =
    1000


oneMinute : Milliseconds
oneMinute =
    -- 60 * 1000
    60000


oneHour : Milliseconds
oneHour =
    -- 60 * 60 * 1000
    3600000


oneDay : Milliseconds
oneDay =
    -- 24 * 60 * 60 * 1000
    86400000


oneYear : Milliseconds
oneYear =
    -- 365 * 24 * 60 * 60 * 1000
    31536000000
