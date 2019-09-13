module TestLib exposing (..)

import Expect
import Lib
import Test exposing (..)


suite : Test
suite =
    describe "Lib"
        [ describe "parsePermalink"
            [ test "parse github permalink" <|
                \_ ->
                    let
                        msgs =
                            [ "url#L12-L34"
                            , "https://example.com#L12"
                            , "https://example.com"
                            ]
                    in
                    List.map Lib.parsePermalink msgs
                        |> Expect.equal
                            [ Lib.Permalink "url" (Lib.LineRange 12 34)
                            , Lib.Permalink "https://example.com" (Lib.LineRange 12 12)
                            , Lib.Permalink "https://example.com" (Lib.LineRange 0 0)
                            ]
            ]
        , describe "genScript"
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
                        |> Expect.equal [ Lib.LineRange 2 2, Lib.LineRange 3 8, Lib.LineRange 12 456 ]
            , test "parse format L? e.g. L12 -> [12, 12]" <|
                \_ ->
                    let
                        msgs =
                            [ "L3", "L987" ]
                    in
                    List.map Lib.parseLineRange msgs
                        |> Expect.equal [ Lib.LineRange 3 3, Lib.LineRange 987 987 ]
            ]
        ]
