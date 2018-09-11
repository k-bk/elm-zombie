module Game exposing (..)

import Browser
import Browser.Events exposing (Visibility)
import Debug
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Json.Decode as Decode
import List
import Svg exposing (Svg, svg, rect)
import Svg.Attributes as A
import Vector2 as Vec2 exposing (Vec2(..))


-- MODEL


type alias Model =
    { dynamic : List Dynamic
    , player : Dynamic
    , timeDelta : Time
    , timeDeltaHistory : List Time
    , input : List Key
    , focusCheck : Int -- for debug purposes
    }


type alias Id =
    Int


type alias Time =
    Float


type alias Dynamic =
    { position : Vec2
    , speed : Float
    , direction : Vec2
    , width : Float
    , height : Float
    }


initModel : Model
initModel =
    { dynamic = []
    , player = Dynamic Vec2.null 5 Vec2.null 1 3
    , timeDelta = 0
    , timeDeltaHistory = []
    , input = []
    , focusCheck = 0
    }


init : () -> ( Model, Cmd msg )
init _ =
    ( initModel, Cmd.none )



-- UPDATE


type Msg
    = KeyDown Key
    | KeyUp Key
    | Tick Time
    | FocusChange


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

            FocusChange ->
                { model | focusCheck = model.focusCheck + 1 }

            _ ->
                model


updateTime : Msg -> Model -> Model
updateTime msg model =
    case msg of
        Tick timeDelta ->
            { model
                | timeDelta = timeDelta
                , timeDeltaHistory =
                    List.take 15
                        (timeDelta :: model.timeDeltaHistory)
            }

        _ ->
            model


updatePlayer : Msg -> Model -> Model
updatePlayer msg model =
    let
        player =
            model.player

        addToVector key vector =
            case key of
                Arrow direction ->
                    Vec2.add (toVector direction) vector

                _ ->
                    vector

        updateMovementVector list =
            List.foldl addToVector (Vec2.null) list

        scaledMovement =
            updateMovementVector model.input
                |> Vec2.normalize
                |> Vec2.scale player.speed
                |> Vec2.scale model.timeDelta

        updatePosition position =
            { player | position = position }
    in
        { model | player = updatePosition <| Vec2.add player.position scaledMovement }



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
            , Browser.Events.onVisibilityChange (\_ -> FocusChange)
            ]



-- VIEW


view : Model -> Html msg
view model =
    let
        debugFPS =
            toFloat (List.length model.timeDeltaHistory) / List.sum model.timeDeltaHistory

        debugInfo =
            "FPS: " ++ String.fromInt (round debugFPS)
    in
        div
            [ style "width" "640px"
            , style "height" "480px"
            ]
            [ viewMap model
            , Html.text debugInfo
            , Html.text (Debug.toString model)
            ]


viewMap : Model -> Html msg
viewMap model =
    svg [ A.viewBox "0 0 20 15" ]
        [ viewDynamics model ]


viewDynamics : Model -> Svg msg
viewDynamics model =
    let
        allDynamic =
            model.player :: model.dynamic

        box { position, width, height } =
            rect
                [ A.x <| String.fromFloat <| Vec2.getX position
                , A.y <| String.fromFloat <| Vec2.getY <| Vec2.negate position
                , A.width <| String.fromFloat width
                , A.height <| String.fromFloat height
                ]
                []
    in
        svg []
            (List.map box allDynamic)



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
