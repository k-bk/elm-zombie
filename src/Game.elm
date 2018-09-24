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
import Dict exposing (Dict(..))
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Json.Decode as Decode
import List
import Svg exposing (Svg, svg, rect)
import Svg.Attributes as A
import Vector2 as Vec2 exposing (Vec2(..))


-- MODEL


type alias Model =
    { entities : Dict Int Entity
    , timeDelta : Time
    , timeDeltaHistory : List Time
    , input : List Key
    , maxId : Int
    }


type alias Time =
    Float


initModel : Model
initModel =
    let
        addEntity entity model =
            { model
                | maxId = model.maxId + 1
                , entities = Dict.insert (model.maxId + 1) entity model.entities
            }

        player =
            [ Position (Vec2 1 2)
            , Speed 3
            , Direction Vec2.null
            , Size ( 1, 2 )
            , Controllable
            ]

        flyingBird =
            [ Position (Vec2 0 5)
            , Speed 0
            , Direction (Vec2 1 0)
            , Size ( 2, 0.5 )
            ]

        block x y =
            [ Position (Vec2 x y)
            , Speed 0
            , Direction Vec2.null
            , Size ( 1, 1 )
            ]

        emptyModel =
            { entities = Dict.empty
            , timeDelta = 0
            , timeDeltaHistory = []
            , input = []
            , maxId = 0
            }
    in
        emptyModel
            |> addEntity player
            |> addEntity flyingBird
            |> addEntity (block 2 2)
            |> addEntity (block 2 3)
            |> addEntity (block 5 5)
            |> addEntity (block 5 6)
            |> addEntity (block 7 8)


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
            case msg of
                Tick timeDelta ->
                    -- update time sync things
                    model
                        |> updateTime timeDelta
                        |> updateAllControllable
                        |> updateAllDynamic

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

            _ ->
                model


updateTime : Time -> Model -> Model
updateTime timeDelta model =
    { model
        | timeDelta = timeDelta
        , timeDeltaHistory =
            List.take 10 (timeDelta :: model.timeDeltaHistory)
    }


updateAllControllable : Model -> Model
updateAllControllable model =
    let
        newDirection =
            keysToVector model.input
                |> Vec2.normalize
    in
        { model | entities = Dict.map (\_ -> updateControllable newDirection) model.entities }


updateControllable : Vec2 -> Entity -> Entity
updateControllable newDirection entity =
    case
        [ getComponent Component.controllable entity
        , getComponent Component.direction entity
        ]
    of
        [ Just Controllable, Just (Direction direction) ] ->
            updateComponent Component.direction entity (Direction newDirection)

        _ ->
            entity


updateAllDynamic : Model -> Model
updateAllDynamic model =
    { model | entities = Dict.map (\_ -> updateDynamic model.timeDelta) model.entities }


updateDynamic : Time -> Entity -> Entity
updateDynamic timeDelta entity =
    case
        [ getComponent Component.position entity
        , getComponent Component.speed entity
        , getComponent Component.direction entity
        ]
    of
        [ Just (Position position), Just (Speed speed), Just (Direction direction) ] ->
            direction
                |> Vec2.scale speed
                |> Vec2.scale timeDelta
                |> Vec2.add position
                |> Position
                |> updateComponent Component.position entity

        _ ->
            entity



{-

   resolveCollision : Time -> Dict Int Entity -> Dict Int Entity
   resolveCollision timeDelta entities =
       let
           makeCheckList =
               Dict.map (\x -> ( True, x )) entities

           canMove dt entity list =
               not <| List.any (\e -> isColliding dt entity (Tuple.second e)) list

           move dt checkList ( hasToMove, entity ) =
               if hasToMove then
                   if canMove dt entity checkList then
                       ( True, updateDynamic dt entity )
                   else
                       ( True, entity )
               else
                   ( False, entity )

           resolveList dt checkList =
               let
                   newList =
                       List.map (move dt checkList) checkList
               in
                   if dt < 0.001 then
                       newList
                   else if List.any Tuple.first newList then
                       resolveList (dt / 2) newList
                   else
                       newList
       in
           resolveList timeDelta makeCheckList
               |> List.map Tuple.second


   isColliding : Time -> Entity -> Entity -> Bool
   isColliding timeDelta entityA entityB =
       let
           newPosition x =
               x.direction
                   |> Vec2.scale x.speed
                   |> Vec2.scale timeDelta
                   |> Vec2.add x.position
       in
           case ( getCollider entityA, getCollider entityB ) of
               ( Just a, Just b ) ->
                   let
                       newA =
                           newPosition a

                       newB =
                           newPosition b
                   in
                       if a == b then
                           False
                       else
                           (Vec2.getX newA < Vec2.getX newB + Tuple.first b.size)
                               && (Vec2.getX newA + Tuple.first a.size > Vec2.getX newB)
                               && (Vec2.getY newA < Vec2.getY newB + Tuple.second b.size)
                               && (Vec2.getY newA + Tuple.second a.size > Vec2.getY newB)

               _ ->
                   False
-}


type alias Collider =
    { position : Vec2
    , speed : Float
    , direction : Vec2
    , size : ( Float, Float )
    }


getCollider : Entity -> Maybe Collider
getCollider entity =
    case
        [ getComponent Component.position entity
        , getComponent Component.size entity
        , getComponent Component.speed entity
        , getComponent Component.direction entity
        ]
    of
        [ Just (Position position), Just (Size ( width, height )), Just (Speed speed), Just (Direction direction) ] ->
            Just <|
                { position = position
                , size = ( width, height )
                , direction = direction
                , speed = speed
                }

        _ ->
            Nothing



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
            , Html.text (Debug.toString model.input)
            ]


viewMap : Model -> Html msg
viewMap model =
    svg [ A.viewBox "0 0 20 15" ]
        (Dict.values <| Dict.map (\_ -> viewBox) model.entities)


viewBox : Entity -> Svg msg
viewBox entity =
    case
        [ getComponent Component.position entity
        , getComponent Component.size entity
        ]
    of
        [ Just (Position position), Just (Size ( width, height )) ] ->
            rect
                [ A.x <| String.fromFloat <| Vec2.getX position
                , A.y <| String.fromFloat <| Vec2.getY position
                , A.width <| String.fromFloat width
                , A.height <| String.fromFloat height
                ]
                []

        _ ->
            svg [] []



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
