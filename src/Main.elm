port module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Lib


type alias Model =
    { permalink : String
    , footerType : Lib.FooterType
    }


init : Int -> ( Model, Cmd Msg )
init _ =
    ( { permalink = ""
      , footerType = Lib.Default
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = InputPermalink String
    | ChangeFooterType String
    | Copy


port copy : () -> Cmd msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputPermalink text ->
            ( { model | permalink = text }, Cmd.none )

        ChangeFooterType text ->
            ( { model | footerType = Lib.footerTypeFromString text }, Cmd.none )

        Copy ->
            ( model, copy () )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


srcInput : Model -> Html Msg
srcInput model =
    input [ type_ "text", placeholder "Input Permalink", value model.permalink, onInput InputPermalink ] []


onChange : (String -> msg) -> Attribute msg
onChange handler =
    on "change" (Json.map handler targetValue)


distTextarea : String -> Html Msg
distTextarea dist =
    textarea [ id "result", readonly True ] [ text dist ]


footerTypes : List Lib.FooterType
footerTypes =
    [ Lib.Default, Lib.Minimal, Lib.No ]


footerOption : Lib.FooterType -> Html Msg
footerOption ft =
    let
        val =
            Lib.footerTypeToString ft
    in
    option [ value val ] [ text val ]


footerSelect : Model -> Html Msg
footerSelect model =
    select [ value (Lib.footerTypeToString model.footerType), onChange ChangeFooterType ]
        (List.map footerOption footerTypes)


view : Model -> Html Msg
view model =
    let
        script =
            Lib.convertGitItScript model.permalink model.footerType
    in
    div [ id "app" ]
        [ srcInput model
        , distTextarea script
        , div [ class "button-block" ]
            [ div [ class "footer-select-block" ]
                [ span [] [ text "Footer: " ]
                , footerSelect model
                ]
            , button [ onClick Copy ] [ text "copy" ]
            ]
        ]



-- INIT


main : Program Int Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
