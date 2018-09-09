module Game exposing (..)

import Browser
import Browser.Events
import Html exposing (Html, div, text)
import Vector2 as Vec2 exposing (Vec2(..))


-- MODEL


type alias Model =
    { player : Player
    , oldModel : Model
    , time : Time
    }


type alias Player =
    { position : Vec2
    }


type alias Movable =
    { position : Vec2
    , speed : Float
    }


initModel : Model
initModel =
    let
        initPlayer =
            { position = Vec2 0 0 }
    in
        { player = initPlayer
        }


init : () -> ( Model, Cmd msg )
init _ =
    ( initModel, Cmd.none )



-- UPDATE


type Msg
    = Input Button
    | Tick Time


type Button
    = Arrow Direction
    | Shoot
    | Other


type alias Time =
    Float


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        Input button ->
            ( { model | input = button :: model.input }, Cmd.none )

        Tick time ->
            ( { model | oldModel = model, time = time }, Cmd.none )


move : Movable -> Vec2 -> Time -> Movable
move movable direction dt =
    { movable
        | position = Vec2.scale (movable.speed * dt) direction
    }


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
    let
        player =
            model.player
    in
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



-- DIRECTION


type Direction
    = Up
    | Down
    | Left
    | Right


toVector : Direction -> Vec2
toVector dir =
    case dir of
        Up ->
            Vec2 0 1

        Down ->
            Vec2 0 -1

        Left ->
            Vec2 -1 0

        Right ->
            Vec2 1 0
