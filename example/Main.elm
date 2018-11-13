module Main exposing (main)

import Browser
import Calendar
import Calendar.Day as Day exposing (Day)
import Calendar.Month as Month exposing (Month)
import Calendar.Year as Year exposing (Year)
import Clock
import Clock.Hour as Hour exposing (Hour)
import Clock.Minute as Minute exposing (Minute)
import Clock.Second as Second exposing (Second)
import DateTime exposing (DateTime)
import Html exposing (Html, br, div, text)
import Json.Decode as Decode exposing (Decoder)
import Parser as Parse exposing ((|.), (|=), Parser)
import Task exposing (Task)
import Time


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Flags =
    ()


type alias Model =
    Maybe DateTime


initialModel : Model
initialModel =
    Nothing


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( initialModel
    , Task.perform SetDateTime getDateTime
    )


type Msg
    = SetDateTime DateTime


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        SetDateTime dateTime ->
            ( Just dateTime, Cmd.none )


view : Model -> Html Msg
view model =
    case model of
        Nothing ->
            viewDunno

        Just dateTime ->
            viewDateTime dateTime


viewDunno : Html Msg
viewDunno =
    div []
        [ text "¯\\_(ツ)_/¯" ]


viewDateTime : DateTime -> Html Msg
viewDateTime dateTime =
    div []
        [ text "toIsoString: "
        , text (toIsoString dateTime)
        , br [] []
        , text "round tripped: "
        , text <|
            Maybe.withDefault "oops!" <|
                Maybe.map toIsoString (fromIsoString (toIsoString dateTime))
        ]


{-| Task for getting the current `DateTime` in the current `Time.Zone`.
-}
getDateTime : Task Never DateTime
getDateTime =
    Time.here
        |> Task.andThen
            (\zone ->
                Time.now
                    |> Task.map
                        (DateTime.fromPosix zone)
            )


{-| Convert a `DateTime` to `"yyyy-mm-ddTHH:MM:SSZ"` format.
-}
toIsoString : DateTime -> String
toIsoString dateTime =
    let
        dateString : String
        dateString =
            String.join "-" [ yearString, monthString, dayString ]

        yearString : String
        yearString =
            dateTime
                |> DateTime.year
                |> Year.toInt
                |> String.fromInt

        monthString : String
        monthString =
            dateTime
                |> DateTime.month
                |> Month.toInt
                |> String.fromInt
                |> String.pad 2 '0'

        dayString : String
        dayString =
            dateTime
                |> DateTime.day
                |> Day.toInt
                |> String.fromInt
                |> String.pad 2 '0'

        timeString : String
        timeString =
            String.join ":" [ hourString, minuteString, secondString ]

        hourString : String
        hourString =
            dateTime
                |> DateTime.hour
                |> Hour.toInt
                |> String.fromInt
                |> String.pad 2 '0'

        minuteString : String
        minuteString =
            dateTime
                |> DateTime.minute
                |> Minute.toInt
                |> String.fromInt
                |> String.pad 2 '0'

        secondString : String
        secondString =
            dateTime
                |> DateTime.second
                |> Second.toInt
                |> String.fromInt
                |> String.pad 2 '0'
    in
    dateString ++ "T" ++ timeString ++ "Z"


{-| Try and get a `DateTime` from a `"yyyy-mm-ddTHH:MM:SSZ"` string.
-}
fromIsoString : String -> Maybe DateTime
fromIsoString string =
    case Parse.run parseIsoString string of
        Err err ->
            Debug.log (Debug.toString err) Nothing

        Ok dateTime ->
            Just dateTime


{-| Decode `DateTime` from a `"yyyy-mm-ddTHH:MM:SSZ"` string.
-}
decodeIsoString : Decoder DateTime
decodeIsoString =
    Decode.string
        |> Decode.andThen
            (\string ->
                case Parse.run parseIsoString string of
                    Err _ ->
                        Decode.fail ("unknown date format: " ++ string)

                    Ok dateTime ->
                        Decode.succeed dateTime
            )


{-| Parse a `DateTime` from a `"yyyy-mm-ddTHH:MM:SSZ"` string.
-}
parseIsoString : Parser DateTime
parseIsoString =
    let
        parseDate : Parser Calendar.Date
        parseDate =
            Parse.succeed triple
                |= parseYear
                |. Parse.symbol "-"
                |= parseMonth
                |. Parse.symbol "-"
                |= parseDay
                |> Parse.andThen
                    (\( y, m, d ) ->
                        case Calendar.fromYearMonthDay y m d of
                            Nothing ->
                                Parse.problem
                                    ("invalid date: "
                                        ++ String.join "-"
                                            [ String.fromInt (Year.toInt y)
                                            , String.fromInt (Month.toInt m)
                                            , String.fromInt (Day.toInt d)
                                            ]
                                    )

                            Just date ->
                                Parse.succeed date
                    )

        parseYear : Parser Year
        parseYear =
            parseIntLike "year" Year.fromInt

        parseMonth : Parser Month
        parseMonth =
            parseIntLike "month" Month.fromInt

        parseDay : Parser Day
        parseDay =
            parseIntLike "day" Day.fromInt

        parseTime : Parser Clock.Time
        parseTime =
            Parse.succeed Clock.Time
                |= parseHour
                |. Parse.symbol ":"
                |= parseMinute
                |. Parse.symbol ":"
                |= parseSecond
                |= Parse.oneOf
                    [ Parse.symbol "." |> Parse.andThen (\_ -> Parse.int)
                    , Parse.succeed 0
                    ]

        parseHour : Parser Hour
        parseHour =
            parseIntLike "hour" Hour.fromInt

        parseMinute : Parser Minute
        parseMinute =
            parseIntLike "minute" Minute.fromInt

        parseSecond : Parser Second
        parseSecond =
            parseIntLike "second" Second.fromInt
    in
    Parse.succeed DateTime.fromUtcDateAndTime
        |= parseDate
        |. Parse.symbol "T"
        |= parseTime
        |. Parse.symbol "Z"


parseIntLike : String -> (Int -> Maybe a) -> Parser a
parseIntLike name read =
    parsePaddedInt
        |> Parse.andThen
            (\int ->
                case read int of
                    Nothing ->
                        Parse.problem ("invalid " ++ name ++ ": " ++ String.fromInt int)

                    Just year ->
                        Parse.succeed year
            )


parsePaddedInt : Parser Int
parsePaddedInt =
    Parse.chompWhile isZero |> Parse.andThen (\_ -> Parse.int)


isZero : Char -> Bool
isZero c =
    c == '0'


triple : a -> b -> c -> ( a, b, c )
triple a b c =
    ( a, b, c )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
