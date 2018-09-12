module Game exposing (..)

import Browser
import Browser.Events exposing (Visibility)
import Component
    exposing
        ( Entity
        , Component(..)
        , getComponent
        , updateComponent
        , hasComponent
        )
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
    { entities : List Entity
    , timeDelta : Time
    , timeDeltaHistory : List Time
    , input : List Key
    , focusCheck : Int -- for debug purposes
    }


type alias Id =
    Int


type alias Time =
    Float


initModel : Model
initModel =
    let
        player : Entity
        player =
            ( 1
            , [ Position (Vec2 1 2)
              , Speed 3
              , Direction Vec2.null
              , Width 1
              , Height 2
              , Controllable
              ]
            )
    in
        { entities = [ player ]
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


keysToVector : List Key -> Vec2
keysToVector keyList =
    let
        addToVector key vector =
            case key of
                Arrow direction ->
                    Vec2.add (toVector direction) vector

                _ ->
                    vector
    in
        List.foldl addToVector (Vec2.null) keyList


updatePlayer : Msg -> Model -> Model
updatePlayer msg model =
    let
        movement =
            keysToVector model.input
                |> Vec2.normalize
                |> Vec2.scale model.timeDelta
    in
        { model
            | entities =
                List.map (updateControllable movement) model.entities
        }


updateControllable : Vec2 -> Entity -> Entity
updateControllable movement entity =
    case
        ( getComponent Component.position entity
        , getComponent Component.speed entity
        , getComponent Component.controllable entity
        )
    of
        ( Just (Position position), Just (Speed speed), Just Controllable ) ->
            Vec2.scale speed movement
                |> Vec2.add position
                |> Position
                |> updateComponent Component.position entity

        _ ->
            entity



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
        (onlyValues <| List.map viewBox model.entities)


viewBox : Entity -> Maybe (Svg msg)
viewBox entity =
    case
        ( getComponent Component.position entity
        , getComponent Component.width entity
        , getComponent Component.height entity
        )
    of
        ( Just (Position position), Just (Width width), Just (Height height) ) ->
            Just
                (rect
                    [ A.x <| String.fromFloat <| Vec2.getX position
                    , A.y <| String.fromFloat <| Vec2.getY <| Vec2.negate position
                    , A.width <| String.fromFloat width
                    , A.height <| String.fromFloat height
                    ]
                    []
                )

        _ ->
            Nothing



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



-- OTHER


onlyValues : List (Maybe a) -> List a
onlyValues list =
    case list of
        (Just value) :: tail ->
            value :: onlyValues tail

        Nothing :: tail ->
            onlyValues tail

        [] ->
            []
