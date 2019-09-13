module Lib exposing (..)


type alias Permalink =
    { url : String
    , lineRange : LineRange
    }


type alias LineRange =
    { from : Int
    , to : Int
    }


type FooterType
    = Default
    | Minimal
    | No


footerTypeToString : FooterType -> String
footerTypeToString ft =
    case ft of
        Default ->
            "Default"

        Minimal ->
            "Minimal"

        No ->
            "No"


footerTypeFromString : String -> FooterType
footerTypeFromString str =
    if str == footerTypeToString Default then
        Default

    else if str == footerTypeToString Minimal then
        Minimal

    else
        No


convertGitItScript : String -> FooterType -> String
convertGitItScript msg ft =
    convertGitItFile (parsePermalink msg) ft
        |> genScript


toFooterQuery : FooterType -> String
toFooterQuery footerType =
    case footerType of
        Default ->
            ""

        Minimal ->
            "footer=minimal"

        No ->
            "footer=no"


toLineRangeQuery : LineRange -> String
toLineRangeQuery lineRange =
    if lineRange.from == 0 then
        ""

    else
        "slice="
            ++ String.fromInt (lineRange.from - 1)
            ++ (if lineRange.from == lineRange.to then
                    ""

                else
                    ":" ++ String.fromInt lineRange.to
               )


convertGitItFile : Permalink -> FooterType -> String
convertGitItFile msg ft =
    let
        footerTypeQuery =
            toFooterQuery ft

        lineRangeQuery =
            toLineRangeQuery msg.lineRange

        queryList =
            List.filter (\s -> String.length s > 0) [ lineRangeQuery, footerTypeQuery ]
    in
    if List.length queryList == 0 then
        msg.url

    else
        msg.url ++ "?" ++ String.join "&" queryList


gitItScriptPre : String
gitItScriptPre =
    "<script src=\"https://gist-it.appspot.com/"


gitItScriptSuf : String
gitItScriptSuf =
    "\"></script>"


genScript : String -> String
genScript msg =
    gitItScriptPre ++ msg ++ gitItScriptSuf


parsePermalink : String -> Permalink
parsePermalink msg =
    let
        splited =
            String.split "#" msg
    in
    Permalink (getHeadString splited) (parseLineRange (getHeadString (List.reverse splited)))


parseLineRange : String -> LineRange
parseLineRange msg =
    let
        range =
            String.split "-" msg
                |> List.map (\s -> String.dropLeft 1 s)
                |> List.map String.toInt
                |> List.map getInt
    in
    if List.length range == 1 then
        LineRange (getHeadInt range) (getHeadInt range)

    else
        LineRange (getHeadInt range) (getHeadInt (List.reverse range))


getInt : Maybe Int -> Int
getInt msg =
    case msg of
        Nothing ->
            0

        Just int ->
            int


getHeadInt : List Int -> Int
getHeadInt msg =
    case List.head msg of
        Nothing ->
            0

        Just int ->
            int


getHeadString : List String -> String
getHeadString msg =
    case List.head msg of
        Nothing ->
            "![ERROR]"

        Just str ->
            str
