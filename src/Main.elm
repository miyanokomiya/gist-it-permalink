port module Main exposing (main)

import Browser
import Html
import Html.Attributes
import Html.Events
import Lib


type alias Model =
    { permalink : String
    }


init : Int -> ( Model, Cmd Msg )
init _ =
    ( { permalink = ""
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = InputPermalink String
    | Copy


port copy : () -> Cmd msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputPermalink text ->
            ( { model | permalink = text }, Cmd.none )

        Copy ->
            ( model, copy () )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html.Html Msg
view model =
    let
        script =
            Lib.convertGitItScript model.permalink
    in
    Html.div [ Html.Attributes.id "app" ]
        [ viewInput "text" "Input Permalink" model.permalink InputPermalink
        , Html.textarea [ Html.Attributes.id "result", Html.Attributes.readonly True ] [ Html.text script ]
        , Html.div [ Html.Attributes.class "button-block" ]
            [ Html.button [ Html.Events.onClick Copy ] [ Html.text "copy" ]
            ]
        ]


viewInput : String -> String -> String -> (String -> msg) -> Html.Html msg
viewInput t p v toMsg =
    Html.input [ Html.Attributes.type_ t, Html.Attributes.placeholder p, Html.Attributes.value v, Html.Events.onInput toMsg ] []



-- INIT


main : Program Int Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
