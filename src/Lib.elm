module Lib exposing (..)


type alias Permalink =
    { url : String
    , lineRange : LineRange
    }


type alias LineRange =
    { from : Int
    , to : Int
    }


genScript : String -> String
genScript msg =
    "<script src=\"https://gist-it.appspot.com/https://github.com/" ++ msg ++ "\"></script>"


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
