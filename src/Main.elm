port module Main exposing (main)

import Browser
import Html
import Html.Events


type alias Model =
    {}


init : Int -> ( Model, Cmd Msg )
init now =
    ( {}
    , Cmd.none
    )



-- UPDATE


type Msg
    = Tmp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tmp ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html.Html Msg
view model =
    Html.div []
        []



-- INIT


main : Program Int Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
