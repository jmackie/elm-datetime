module Clock.Internal exposing
    ( Hour(..)
    , Millisecond(..)
    , Minute(..)
    , Raw
    , Second(..)
    , Time(..)
    , TimeRecord
    , compare
    , compareHour
    , compareMillis
    , compareMinute
    , compareSecond
    , decrementHour
    , decrementMillis
    , decrementMinute
    , decrementSecond
    , fromPosix
    , fromRaw
    , getHour
    , getMillis
    , getMinute
    , getSecond
    , hourFromInt
    , hourToInt
    , incrementHour
    , incrementMillis
    , incrementMinute
    , incrementSecond
    , millisFromInt
    , millisInAMinute
    , millisInASecond
    , millisInAnHour
    , millisToInt
    , minuteFromInt
    , minuteToInt
    , secondFromInt
    , secondToInt
    , toMillis
    , zero
    )

import Common exposing (applyMaybe, boundedInt, comparing, foldOrders)
import Time


type Time
    = Time TimeRecord


type alias TimeRecord =
    { hour : Hour
    , minute : Minute
    , second : Second
    , millis : Millisecond
    }


getHour : Time -> Hour
getHour (Time { hour }) =
    hour


getMinute : Time -> Minute
getMinute (Time { minute }) =
    minute


getSecond : Time -> Second
getSecond (Time { second }) =
    second


getMillis : Time -> Millisecond
getMillis (Time { millis }) =
    millis


fromPosix : Time.Posix -> Time
fromPosix posix =
    let
        -- Remember: POSIX time is the number of seconds
        -- that have elapsed since 1970-01-01T00:00:00Z
        zone =
            Time.utc
    in
    Time
        { hour = Hour (Time.toHour zone posix)
        , minute = Minute (Time.toMinute zone posix)
        , second = Second (Time.toSecond zone posix)
        , millis = Millisecond (Time.toMillis zone posix)
        }


fromRaw : Raw -> Maybe Time
fromRaw { hour, minute, second, millis } =
    Maybe.map Time
        (Just TimeRecord
            |> applyMaybe (hourFromInt hour)
            |> applyMaybe (minuteFromInt minute)
            |> applyMaybe (secondFromInt second)
            |> applyMaybe (millisFromInt millis)
        )


type alias Raw =
    { hour : Int
    , minute : Int
    , second : Int
    , millis : Int
    }


toMillis : Time -> Int
toMillis (Time { hour, minute, second, millis }) =
    List.sum
        [ hourToInt hour * millisInAnHour
        , minuteToInt minute * millisInAMinute
        , secondToInt second * millisInASecond
        , millisToInt millis
        ]


compare : Time -> Time -> Order
compare lhs rhs =
    foldOrders
        (comparing (getHour >> hourToInt) lhs rhs)
        [ comparing (getMinute >> minuteToInt) lhs rhs
        , comparing (getSecond >> secondToInt) lhs rhs
        , comparing (getMillis >> millisToInt) lhs rhs
        ]


compareHour : Hour -> Hour -> Order
compareHour =
    comparing hourToInt


compareMinute : Minute -> Minute -> Order
compareMinute =
    comparing minuteToInt


compareSecond : Second -> Second -> Order
compareSecond =
    comparing secondToInt


compareMillis : Millisecond -> Millisecond -> Order
compareMillis =
    comparing millisToInt


zero : Time
zero =
    Time
        { hour = Hour 0
        , minute = Minute 0
        , second = Second 0
        , millis = Millisecond 0
        }


type Hour
    = Hour Int


hourToInt : Hour -> Int
hourToInt (Hour hour) =
    hour


hourFromInt : Int -> Maybe Hour
hourFromInt =
    boundedInt Hour { lower = 0, upper = 24 }


type Minute
    = Minute Int


minuteToInt : Minute -> Int
minuteToInt (Minute minute) =
    minute


minuteFromInt : Int -> Maybe Minute
minuteFromInt =
    boundedInt Minute { lower = 0, upper = 60 }


type Second
    = Second Int


secondToInt : Second -> Int
secondToInt (Second second) =
    second


secondFromInt : Int -> Maybe Second
secondFromInt =
    boundedInt Second { lower = 0, upper = 60 }


type Millisecond
    = Millisecond Int


millisToInt : Millisecond -> Int
millisToInt (Millisecond millis) =
    millis


millisFromInt : Int -> Maybe Millisecond
millisFromInt =
    boundedInt Millisecond { lower = 0, upper = 1000 }


incrementHour : Time -> ( Time, Bool )
incrementHour (Time time) =
    let
        incremented =
            hourToInt time.hour + 1
    in
    if incremented >= 24 then
        ( Time { time | hour = Hour 0 }
        , True
        )

    else
        ( Time { time | hour = Hour incremented }
        , False
        )


incrementMinute : Time -> ( Time, Bool )
incrementMinute (Time time) =
    let
        incremented =
            minuteToInt time.minute + 1
    in
    if incremented >= 60 then
        incrementHour (Time { time | minute = Minute 0 })

    else
        ( Time { time | minute = Minute incremented }
        , False
        )


incrementSecond : Time -> ( Time, Bool )
incrementSecond (Time time) =
    let
        incremented =
            secondToInt time.second + 1
    in
    if incremented >= 60 then
        incrementMinute (Time { time | second = Second 0 })

    else
        ( Time { time | second = Second incremented }
        , False
        )


incrementMillis : Time -> ( Time, Bool )
incrementMillis (Time time) =
    let
        incremented =
            millisToInt time.millis + 1
    in
    if incremented >= 1000 then
        incrementSecond (Time { time | millis = Millisecond 0 })

    else
        ( Time { time | millis = Millisecond incremented }
        , False
        )


decrementHour : Time -> ( Time, Bool )
decrementHour (Time time) =
    let
        decremented =
            hourToInt time.hour - 1
    in
    if decremented < 0 then
        ( Time { time | hour = Hour 23 }
        , True
        )

    else
        ( Time { time | hour = Hour decremented }
        , False
        )


decrementMinute : Time -> ( Time, Bool )
decrementMinute (Time time) =
    let
        decremented =
            minuteToInt time.minute - 1
    in
    if decremented < 0 then
        decrementHour (Time { time | minute = Minute 59 })

    else
        ( Time { time | minute = Minute decremented }
        , False
        )


decrementSecond : Time -> ( Time, Bool )
decrementSecond (Time time) =
    let
        decremented =
            secondToInt time.second - 1
    in
    if decremented < 0 then
        decrementMinute (Time { time | second = Second 59 })

    else
        ( Time { time | second = Second decremented }
        , False
        )


decrementMillis : Time -> ( Time, Bool )
decrementMillis (Time time) =
    let
        decremented =
            millisToInt time.millis - 1
    in
    if decremented < 0 then
        decrementSecond (Time { time | millis = Millisecond 999 })

    else
        ( Time { time | millis = Millisecond decremented }
        , False
        )


millisInAnHour : Int
millisInAnHour =
    1000 * 60 * 60


millisInAMinute : Int
millisInAMinute =
    1000 * 60


millisInASecond : Int
millisInASecond =
    1000
