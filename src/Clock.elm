module Clock exposing
    ( Time
    , fromRawParts, fromPosix
    , toMillis
    )

{-| A clock time.


# Type definition

@docs Time


# Creating values

@docs fromRawParts, fromPosix


# Conversions

@docs toMillis

-}

import Clock.Hour as Hour exposing (Hour)
import Clock.Minute as Minute exposing (Minute)
import Clock.Second as Second exposing (Second)
import Time


{-| A clock time.
-}
type alias Time =
    { hour : Hour
    , minute : Minute
    , second : Second
    , millisecond : Int
    }


{-| Construct a clock `Time` from raw hour, minute, second, millisecond integers.
-}
fromRawParts : Int -> Int -> Int -> Int -> Maybe Time
fromRawParts rawHour rawMinute rawSecond rawMillisecond =
    Hour.fromInt rawHour
        |> Maybe.andThen
            (\hour ->
                Minute.fromInt rawMinute
                    |> Maybe.andThen
                        (\minute ->
                            Second.fromInt rawSecond
                                |> Maybe.andThen
                                    (\second ->
                                        Just
                                            { hour = hour
                                            , minute = minute
                                            , second = second
                                            , millisecond = rawMillisecond
                                            }
                                    )
                        )
            )


{-| Get a clock `Time` from a time zone and posix time.
-}
fromPosix : Time.Zone -> Time.Posix -> Time
fromPosix zone posix =
    { hour = Hour.fromPosix zone posix
    , minute = Minute.fromPosix zone posix
    , second = Second.fromPosix zone posix
    , millisecond = Time.toMillis zone posix
    }


{-| Convert a `Time` to milliseconds.

TODO

-}
toMillis : Time -> Int
toMillis _ =
    0
