module TestLib exposing (suite)

import Expect
import Lib
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Lib"
        [ describe "convertGitItScript"
            [ test "convert permalink to git-it script" <|
                \_ ->
                    let
                        msgs =
                            [ { url = "url#L12-L34", footer = Lib.Default }
                            , { url = "https://example.com#L12", footer = Lib.Default }
                            , { url = "https://example.com", footer = Lib.Default }
                            , { url = "url#L12-L34", footer = Lib.No }
                            , { url = "url#L12", footer = Lib.No }
                            , { url = "url", footer = Lib.No }
                            ]
                    in
                    List.map (\m -> Lib.convertGitItScript m.url m.footer) msgs
                        |> Expect.equal
                            (List.map
                                (\s -> Lib.gitItScriptPre ++ s ++ Lib.gitItScriptSuf)
                                [ "url?slice=11:34"
                                , "https://example.com?slice=11"
                                , "https://example.com"
                                , "url?slice=11:34&footer=no"
                                , "url?slice=11&footer=no"
                                , "url?footer=no"
                                ]
                            )
            ]
        , describe "footerTypeToString"
            [ test "footer type to string" <|
                \_ ->
                    let
                        msgs =
                            [ Lib.Default
                            , Lib.Minimal
                            , Lib.No
                            ]
                    in
                    List.map Lib.footerTypeToString msgs
                        |> Expect.equal
                            [ "Default"
                            , "Minimal"
                            , "No"
                            ]
            ]
        , describe "footerTypeFromString"
            [ test "create footer type from string" <|
                \_ ->
                    let
                        msgs =
                            [ "Default"
                            , "Minimal"
                            , "No"
                            , "Unknown"
                            ]
                    in
                    List.map Lib.footerTypeFromString msgs
                        |> Expect.equal
                            [ Lib.Default
                            , Lib.Minimal
                            , Lib.No
                            , Lib.Default
                            ]
            ]
        , describe "toFooterQuery"
            [ test "footer type to query" <|
                \_ ->
                    let
                        msgs =
                            [ Lib.Default
                            , Lib.Minimal
                            , Lib.No
                            ]
                    in
                    List.map Lib.toFooterQuery msgs
                        |> Expect.equal
                            [ ""
                            , "footer=minimal"
                            , "footer=no"
                            ]
            ]
        , describe "toLineRangeQuery"
            [ test "line lange to query" <|
                \_ ->
                    let
                        msgs =
                            [ Lib.LineRange 12 34
                            , Lib.LineRange 12 12
                            , Lib.LineRange 0 0
                            ]
                    in
                    List.map Lib.toLineRangeQuery msgs
                        |> Expect.equal
                            [ "slice=11:34"
                            , "slice=11"
                            , ""
                            ]
            ]
        , describe "convertGitItFile"
            [ test "convert inline url & query" <|
                \_ ->
                    let
                        msgs =
                            [ Lib.Permalink "url" (Lib.LineRange 12 34)
                            , Lib.Permalink "https://example.com" (Lib.LineRange 12 12)
                            , Lib.Permalink "https://example.com" (Lib.LineRange 0 0)
                            ]

                        footerType =
                            Lib.Default
                    in
                    List.map (\m -> Lib.convertGitItFile m footerType) msgs
                        |> Expect.equal
                            [ "url?slice=11:34"
                            , "https://example.com?slice=11"
                            , "https://example.com"
                            ]
            ]
        , describe "parsePermalink"
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
                        |> Expect.equal
                            (Lib.gitItScriptPre
                                ++ "$msg$"
                                ++ Lib.gitItScriptSuf
                            )
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
