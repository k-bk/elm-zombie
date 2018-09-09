module Game exposing (..)

import Browser
import Browser.Events
import Html exposing (Html, div, text)
import List
import Dict exposing (Dict)
import Vector2 as Vec2 exposing (Vec2(..))
import Time


-- MODEL


type alias Model =
    { movable : List Movable
    , timeDelta : Time
    }


type alias Id =
    Int


type alias Time =
    Float


type alias Movable =
    { position : Vec2
    , speed : Float
    , direction : Vec2
    , commands : List Command
    }


type Command
    = Move Vec2


initModel : Model
initModel =
    { movable = []
    , timeDelta = 0
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
    | Other


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        Input button ->
            ( model, Cmd.none )

        Tick timeDelta ->
            ( { model | timeDelta = timeDelta }, Cmd.none )


manageInput : Button -> Model -> Model
manageInput button model =
    case button of
        Arrow direction ->
            model

        Other ->
            model



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        newTick delta =
            Tick (delta / 1000)
    in
        Sub.batch
            [ Browser.Events.onAnimationFrameDelta newTick
            ]



-- VIEW


view : Model -> Html msg
view model =
    let
        debugInfo =
            "dt: " ++ String.fromFloat model.timeDelta
    in
        div []
            [ Html.text debugInfo
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
