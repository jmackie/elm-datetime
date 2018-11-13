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

    > fromRawParts 12 30 0 0 |> Maybe.map (\t -> (t.hour, t.minute))
    Just (Hour 12,Minute 30) : Maybe ( Hour, Minute )

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

    > fromPosix Time.utc (Time.millisToPosix 0)
    { hour = Hour 0, millisecond = 0, minute = Minute 0, second = Second 0 } : Time

-}
fromPosix : Time.Zone -> Time.Posix -> Time
fromPosix zone posix =
    { hour = Hour.fromPosix zone posix
    , minute = Minute.fromPosix zone posix
    , second = Second.fromPosix zone posix
    , millisecond = Time.toMillis zone posix
    }


{-| Convert a `Time` to milliseconds.

    > fromRawParts 12 30 0 0 |> Maybe.map toMillis
    Just 45000000 : Maybe Int

-}
toMillis : Time -> Int
toMillis { hour, minute, second, millisecond } =
    List.sum
        [ Hour.toInt hour * 3600000
        , Minute.toInt minute * 60000
        , Second.toInt second * 1000
        , millisecond
        ]
