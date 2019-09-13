module TestLib exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Lib
import Test exposing (..)


suite : Test
suite =
    describe "Lib"
        [ describe "genScript"
            [ test "generate gist-it script" <|
                \_ ->
                    let
                        msg =
                            "$msg$"
                    in
                    Lib.genScript msg
                        |> Expect.equal "<script src=\"https://gist-it.appspot.com/https://github.com/$msg$\"></script>"
            ]
        , describe "parseLineRange"
            [ test "parse format L?-L? e.g. L12-L456 -> [12, 456]" <|
                \_ ->
                    let
                        msgs =
                            [ "L2-L2", "L3-L8", "L12-L456" ]
                    in
                    List.map Lib.parseLineRange msgs
                        |> Expect.equal [ [ 2, 2 ], [ 3, 8 ], [ 12, 456 ] ]
            , test "parse format L? e.g. L12 -> [12, 12]" <|
                \_ ->
                    let
                        msgs =
                            [ "L3", "L987" ]
                    in
                    List.map Lib.parseLineRange msgs
                        |> Expect.equal [ [ 3, 3 ], [ 987, 987 ] ]
            ]
        ]
