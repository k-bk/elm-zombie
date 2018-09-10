module Game exposing (..)

import Browser
import Browser.Events
import Debug
import Html exposing (Html, div, text)
import Json.Decode as Decode
import List
import Vector2 as Vec2 exposing (Vec2(..))


-- MODEL


type alias Model =
    { dynamic : List Dynamic
    , timeDelta : Time
    , input : List Key
    , movementVector : Vec2
    }


type alias Id =
    Int


type alias Time =
    Float


type alias Dynamic =
    { position : Vec2
    , speed : Float
    , direction : Vec2
    }


initModel : Model
initModel =
    { dynamic = []
    , timeDelta = 0
    , input = []
    , movementVector = Vec2.null
    }


init : () -> ( Model, Cmd msg )
init _ =
    ( initModel, Cmd.none )



-- UPDATE


type Msg
    = KeyDown Key
    | KeyUp Key
    | Tick Time


type Key
    = Arrow Direction
    | Other


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    let
        newModel =
            model
                |> updateInput msg
                |> updateTime msg
                |> updatePlayer msg
    in
        ( newModel, Cmd.none )


updateInput : Msg -> Model -> Model
updateInput msg model =
    let
        removeKey key =
            List.filter (\x -> x /= key)

        addKey key list =
            if List.member key list then
                list
            else
                key :: list
    in
        case msg of
            KeyDown key ->
                { model | input = addKey key model.input }

            KeyUp key ->
                { model | input = removeKey key model.input }

            _ ->
                model


updateTime : Msg -> Model -> Model
updateTime msg model =
    case msg of
        Tick timeDelta ->
            { model | timeDelta = timeDelta }

        _ ->
            model


updatePlayer : Msg -> Model -> Model
updatePlayer msg model =
    let
        addToVector key vector =
            case key of
                Arrow direction ->
                    Vec2.add (toVector direction) vector

                _ ->
                    vector

        updateMovementVector list =
            List.foldl addToVector (Vec2.null) list
    in
        { model | movementVector = updateMovementVector model.input }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        newTick delta =
            Tick (delta / 1000)

        toInput string =
            case string of
                "ArrowUp" ->
                    Arrow Up

                "ArrowDown" ->
                    Arrow Down

                "ArrowLeft" ->
                    Arrow Left

                "ArrowRight" ->
                    Arrow Right

                _ ->
                    Other

        decodeKeys =
            Decode.map toInput (Decode.field "key" Decode.string)
    in
        Sub.batch
            [ Browser.Events.onAnimationFrameDelta newTick
            , Sub.map KeyDown <| Browser.Events.onKeyDown decodeKeys
            , Sub.map KeyUp <| Browser.Events.onKeyUp decodeKeys
            ]



-- VIEW


view : Model -> Html msg
view model =
    let
        debugInfo =
            "dt: " ++ String.fromFloat model.timeDelta
    in
        div []
            [ Html.text (Debug.toString model)
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
toVector direction =
    case direction of
        Up ->
            Vec2 0 1

        Down ->
            Vec2 0 -1

        Right ->
            Vec2 1 0

        Left ->
            Vec2 -1 0
