module Clock.Tests exposing (suite)

import Clock
import Clock.Internal as Internal
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Random
import Test exposing (Test)
import Time


suite : Test
suite =
    Test.describe "Clock"
        [ fromPosixTest
        , fromRawTest
        , incrementTest
        , decrementTest
        ]


fuzzOptions : Test.FuzzOptions
fuzzOptions =
    { runs = 1000 }


fromPosixTest : Test
fromPosixTest =
    Test.describe "fromPosix"
        [ Test.fuzzWith fuzzOptions posixFuzzer "returns the correct hour" <|
            \posix ->
                Expect.equal
                    (Time.toHour Time.utc posix)
                    (Clock.hour <| Clock.fromPosix posix)
        , Test.fuzzWith fuzzOptions posixFuzzer "returns the correct minute" <|
            \posix ->
                Expect.equal
                    (Time.toMinute Time.utc posix)
                    (Clock.minute <| Clock.fromPosix posix)
        , Test.fuzzWith fuzzOptions posixFuzzer "returns the correct second" <|
            \posix ->
                Expect.equal
                    (Time.toSecond Time.utc posix)
                    (Clock.second <| Clock.fromPosix posix)
        , Test.fuzzWith fuzzOptions posixFuzzer "returns the correct millisecond" <|
            \posix ->
                Expect.equal
                    (Time.toMillis Time.utc posix)
                    (Clock.millis <| Clock.fromPosix posix)
        ]


fromRawTest : Test
fromRawTest =
    Test.describe "fromRaw"
        [ Test.fuzzWith fuzzOptions validRawFuzzer "preserves hour" <|
            \raw ->
                Expect.equal
                    (Just raw.hour)
                    (Maybe.map Clock.hour <| Clock.fromRaw raw)
        , Test.fuzzWith fuzzOptions validRawFuzzer "preserves minute" <|
            \raw ->
                Expect.equal
                    (Just raw.minute)
                    (Maybe.map Clock.minute <| Clock.fromRaw raw)
        , Test.fuzzWith fuzzOptions validRawFuzzer "preserves second" <|
            \raw ->
                Expect.equal
                    (Just raw.second)
                    (Maybe.map Clock.second <| Clock.fromRaw raw)
        , Test.fuzzWith fuzzOptions validRawFuzzer "preserves millisecond" <|
            \raw ->
                Expect.equal
                    (Just raw.millis)
                    (Maybe.map Clock.millis <| Clock.fromRaw raw)
        , Test.fuzzWith fuzzOptions invalidRawFuzzer "rejects invalid times" <|
            \raw ->
                Expect.equal Nothing (Clock.fromRaw raw)
        ]


compareTest : Test
compareTest =
    Test.describe "compare"
        [ Test.fuzzWith fuzzOptions (Fuzz.tuple ( validRawFuzzer, validRawFuzzer )) "compares correctly" <|
            \( a, b ) ->
                Expect.equal
                    (Just <| compareRaw a b)
                    (Maybe.map2 Clock.compare (Clock.fromRaw a) (Clock.fromRaw b))
        ]


compareRaw : Internal.Raw -> Internal.Raw -> Order
compareRaw a b =
    case compare a.hour b.hour of
        EQ ->
            case compare a.minute b.minute of
                EQ ->
                    case compare a.second b.second of
                        EQ ->
                            compare a.millis b.millis

                        order ->
                            order

                order ->
                    order

        order ->
            order


incrementTest : Test
incrementTest =
    Test.describe "increment*"
        [ Test.fuzzWith fuzzOptions timeFuzzer "incrementHour" <|
            \time ->
                case Clock.incrementHour time of
                    ( _, True ) ->
                        Expect.pass

                    ( incremented, False ) ->
                        Expect.equal
                            Internal.millisInAnHour
                            (Internal.toMillis incremented - Internal.toMillis time)
        , Test.fuzzWith fuzzOptions timeFuzzer "incrementMinute" <|
            \time ->
                case Clock.incrementMinute time of
                    ( _, True ) ->
                        Expect.pass

                    ( incremented, False ) ->
                        Expect.equal
                            Internal.millisInAMinute
                            (Internal.toMillis incremented - Internal.toMillis time)
        , Test.fuzzWith fuzzOptions timeFuzzer "incrementSecond" <|
            \time ->
                case Clock.incrementSecond time of
                    ( _, True ) ->
                        Expect.pass

                    ( incremented, False ) ->
                        Expect.equal
                            Internal.millisInASecond
                            (Internal.toMillis incremented - Internal.toMillis time)
        , Test.fuzzWith fuzzOptions timeFuzzer "incrementMillis" <|
            \time ->
                case Clock.incrementMillis time of
                    ( _, True ) ->
                        Expect.pass

                    ( incremented, False ) ->
                        Expect.equal
                            1
                            (Internal.toMillis incremented - Internal.toMillis time)
        ]


decrementTest : Test
decrementTest =
    Test.describe "decrement*"
        [ Test.fuzzWith fuzzOptions timeFuzzer "decrementHour" <|
            \time ->
                case Clock.decrementHour time of
                    ( _, True ) ->
                        Expect.pass

                    ( decremented, False ) ->
                        Expect.equal
                            Internal.millisInAnHour
                            (Internal.toMillis time - Internal.toMillis decremented)
        , Test.fuzzWith fuzzOptions timeFuzzer "decrementMinute" <|
            \time ->
                case Clock.decrementMinute time of
                    ( _, True ) ->
                        Expect.pass

                    ( decremented, False ) ->
                        Expect.equal
                            Internal.millisInAMinute
                            (Internal.toMillis time - Internal.toMillis decremented)
        , Test.fuzzWith fuzzOptions timeFuzzer "decrementSecond" <|
            \time ->
                case Clock.decrementSecond time of
                    ( _, True ) ->
                        Expect.pass

                    ( decremented, False ) ->
                        Expect.equal
                            Internal.millisInASecond
                            (Internal.toMillis time - Internal.toMillis decremented)
        , Test.fuzzWith fuzzOptions timeFuzzer "decrementMillis" <|
            \time ->
                case Clock.decrementMillis time of
                    ( _, True ) ->
                        Expect.pass

                    ( decremented, False ) ->
                        Expect.equal
                            1
                            (Internal.toMillis time - Internal.toMillis decremented)
        ]



-- FUZZERS --


posixFuzzer : Fuzzer Time.Posix
posixFuzzer =
    Fuzz.intRange 0 Random.maxInt |> Fuzz.map Time.millisToPosix


validRawFuzzer : Fuzzer Internal.Raw
validRawFuzzer =
    Fuzz.map4 Internal.Raw
        (Fuzz.intRange 0 23)
        (Fuzz.intRange 0 59)
        (Fuzz.intRange 0 59)
        (Fuzz.intRange 0 999)


invalidRawFuzzer : Fuzzer Internal.Raw
invalidRawFuzzer =
    Fuzz.map4 Internal.Raw
        (fuzzOutsideIntRange 0 23)
        (fuzzOutsideIntRange 0 59)
        (fuzzOutsideIntRange 0 59)
        (fuzzOutsideIntRange 0 999)


timeFuzzer : Fuzzer Clock.Time
timeFuzzer =
    Fuzz.map
        (\{ hour, minute, second, millis } ->
            Internal.Time
                { hour = Internal.Hour hour
                , minute = Internal.Minute minute
                , second = Internal.Second second
                , millis = Internal.Millisecond millis
                }
        )
        validRawFuzzer


fuzzOutsideIntRange : Int -> Int -> Fuzzer Int
fuzzOutsideIntRange lower upper =
    Fuzz.oneOf
        [ Fuzz.intRange Random.minInt (lower - 1)
        , Fuzz.intRange (upper + 1) Random.maxInt
        ]
