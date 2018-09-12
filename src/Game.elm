module Game exposing (..)

import Browser
import Browser.Events exposing (Visibility)
import Component
    exposing
        ( Entity
        , Component(..)
        , getComponent
        , updateComponent
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
        player =
            ( 1
            , [ Position (Vec2 1 2)
              , Speed 5
              , Movement Vec2.null
              , Width 1
              , Height 2
              , Controllable
              ]
            )

        flyingBird =
            ( 2
            , [ Position (Vec2 0 5)
              , Speed 5
              , Movement (Vec2 1 0)
              , Width 2
              , Height 0.5
              ]
            )
    in
        { entities = [ player, flyingBird ]
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
            case msg of
                Tick timeDelta ->
                    -- update time sync things
                    model
                        |> updateTime msg
                        |> updateControllables msg
                        |> updateDynamics msg

                _ ->
                    -- update frame async things
                    model
                        |> updateInput msg
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


updateControllables : Msg -> Model -> Model
updateControllables msg model =
    let
        newMovement =
            keysToVector model.input
                |> Vec2.normalize
    in
        { model | entities = List.map (updateControllable newMovement) model.entities }


updateDynamics : Msg -> Model -> Model
updateDynamics msg model =
    { model | entities = List.map (updateDynamic model.timeDelta) model.entities }



-- Dynamic entity has Position, Speed and Movement


updateDynamic : Time -> Entity -> Entity
updateDynamic timeDelta entity =
    case
        ( getComponent Component.position entity
        , getComponent Component.speed entity
        , getComponent Component.movement entity
        )
    of
        ( Just (Position position), Just (Speed speed), Just (Movement movement) ) ->
            movement
                |> Vec2.scale speed
                |> Vec2.scale timeDelta
                |> Vec2.add position
                |> Position
                |> updateComponent Component.position entity

        _ ->
            entity



-- Controllable entity has Movement which changes
-- according to input


updateControllable : Vec2 -> Entity -> Entity
updateControllable newMovement entity =
    case
        ( getComponent Component.controllable entity
        , getComponent Component.movement entity
        )
    of
        ( Just Controllable, Just (Movement movement) ) ->
            updateComponent Component.movement entity (Movement newMovement)

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
                    , A.y <| String.fromFloat <| Vec2.getY position
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
            Vec2 0 -1

        Down ->
            Vec2 0 1

        Right ->
            Vec2 1 0

        Left ->
            Vec2 -1 0


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
