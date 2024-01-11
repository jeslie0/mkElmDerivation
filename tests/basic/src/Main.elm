module Main exposing (..)
import Browser
import Html as H exposing (Html)


-- * MAIN


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = viewDocument
        , update = update
        , subscriptions = subscriptions
        }



-- * MODEL


type alias Model =
    { }


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { }
    , Cmd.none
    )



-- * UPDATE


type Msg
    = Foo

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Foo ->
            ({}, Cmd.none)



-- * SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- * VIEW


viewDocument : Model -> Browser.Document Msg
viewDocument model =
    { title = ""
    , body = [ view model ]
    }


view : Model -> Html Msg
view model =
    H.div [] []
