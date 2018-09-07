module Game exposing (..)

import Browser
import Browser.Events
import Html exposing (Html, div, text)


-- MODEL


type alias Model =
    { player : Player }


type alias Player =
    ( Int, Int )


initModel : Model
initModel =
    Model ( 1, 1 )


init : () -> ( Model, Cmd msg )
init _ =
    ( initModel, Cmd.none )



-- UPDATE


type Msg
    = Input Button
    | None


type Button
    = Arrow Direction
    | Shoot
    | Other


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        Input button ->
            ( manageInput button model, Cmd.none )

        None ->
            ( model, Cmd.none )


manageInput : Button -> Model -> Model
manageInput button model =
    case button of
        Arrow direction ->
            model

        Shoot ->
            model

        Other ->
            model



-- SUBSCRIPTIONS


subscriptions : Model -> Sub msg
subscriptions model =
    Sub.batch
        []



-- VIEW


view : Model -> Html msg
view model =
    div []
        [ Html.text "hello"
        ]



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
