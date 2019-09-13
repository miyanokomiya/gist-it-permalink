module Lib exposing (..)


type alias Permalink =
    { url : String
    , from : Int
    , to : Int
    }


genScript : String -> String
genScript msg =
    "<script src=\"https://gist-it.appspot.com/https://github.com/" ++ msg ++ "\"></script>"



-- parsePermalink : String -> Permalink
-- parsePermalink msg =
--   String.split "#" msg
--     |>


parseLineRange : String -> List Int
parseLineRange msg =
    String.split "-" msg
        |> List.map (\s -> String.dropLeft 1 s)
        |> List.map String.toInt
        |> List.map
            (\i ->
                case i of
                    Nothing ->
                        0

                    Just int ->
                        int
            )
        |> completeLineRange


completeLineRange : List Int -> List Int
completeLineRange range =
    if List.length range == 1 then
        [ getHead range, getHead range ]

    else
        range


getHead : List Int -> Int
getHead msg =
    case List.head msg of
        Nothing ->
            0

        Just int ->
            int
