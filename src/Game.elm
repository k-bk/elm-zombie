module Game exposing (..)

import Browser
import Browser.Events exposing (Visibility)
import Debug
import Dict exposing (Dict(..))
import Entity
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Json.Decode as Decode
import List
import Size exposing (Size(..))
import Svg exposing (Svg, svg, rect)
import Svg.Attributes as A
import Vector2 as Vec2 exposing (Vec2(..))


-- MODEL


type alias Model =
    { timeDelta : Time
    , timeDeltaHistory : List Time
    , input : List Key
    , maxId : Int

    -- Components
    , position : Dict Int Vec2
    , speed : Dict Int Float
    , direction : Dict Int Vec2
    , size : Dict Int Size
    , controllable : Dict Int ()
    }


initModel : Model
initModel =
    let
        addPlayer : Model -> Model
        addPlayer model =
            let
                updateComponent component value =
                    Dict.insert (model.maxId + 1) value component
            in
                { model
                    | position = updateComponent model.position <| Vec2 2 4
                    , speed = updateComponent model.speed 5
                    , direction = updateComponent model.direction Vec2.null
                    , size = updateComponent model.size <| Size 1 2
                    , controllable = updateComponent model.controllable ()
                    , maxId = model.maxId + 1
                }

        addBlock : Float -> Float -> Model -> Model
        addBlock x y model =
            let
                updateComponent component value =
                    Dict.insert (model.maxId + 1) value component
            in
                { model
                    | position = updateComponent model.position <| Vec2 x y
                    , size = updateComponent model.size <| Size 2 2
                    , maxId = model.maxId + 1
                }

        emptyModel : Model
        emptyModel =
            { timeDelta = 0
            , timeDeltaHistory = []
            , input = []
            , maxId = 0

            -- Components
            , position = Dict.empty
            , speed = Dict.empty
            , direction = Dict.empty
            , size = Dict.empty
            , controllable = Dict.empty
            }
    in
        emptyModel
            |> addPlayer
            |> addBlock 3 3
            |> addBlock 4 5


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
                        |> updateAllControllable model.input
                        |> updateAllDynamic timeDelta

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


updateAllControllable : List Key -> Controllable a -> Controllable a
updateAllControllable input components =
    let
        newDirection =
            keysToVector input
                |> Vec2.normalize
    in
        { components
            | direction =
                Dict.map (\_ _ -> newDirection)
                    components.controllable
        }


updateAllDynamic : Time -> Dynamic a -> Dynamic a
updateAllDynamic timeDelta components =
    let
        getDynamicList =
            Dict.keys components.position
                |> List.filter (\id -> Dict.member id components.speed)
                |> List.filter (\id -> Dict.member id components.direction)
    in
        List.foldl (updateDynamic timeDelta) components getDynamicList


updateDynamic : Time -> Int -> Dynamic a -> Dynamic a
updateDynamic timeDelta id components =
    case getDynamic components id of
        Just { position, speed, direction } ->
            let
                newPosition =
                    direction
                        |> Vec2.normalize
                        |> Vec2.scale speed
                        |> Vec2.scale timeDelta
                        |> Vec2.add position
            in
                { components
                    | position =
                        components.position |> Dict.insert id newPosition
                }

        Nothing ->
            components


updateAllColliding : Colliding a -> Colliding a -> Colliding a
updateAllColliding old new =
    let
        getCollidingList =
            Dict.keys old.position
                |> List.filter (\id -> Dict.member id old.size)
                |> List.filter (\id -> Dict.member id new.position)
                |> List.filter (\id -> Dict.member id new.size)

        anyCollisionWith id =
            List.any (isColliding new id) getCollidingList
    in
        if List.any anyCollisionWith getCollidingList then
            old
        else
            new


isColliding : Colliding a -> Int -> Int -> Bool
isColliding components idA idB =
    let
        checkAABB (Vec2 xA yA) (Size wA hA) (Vec2 xB yB) (Size wB hB) =
            (xA < xB + wB) && (xA + wA > xB) && (yA < yB + hB) && (yA + hA > yB)
    in
        if idA == idB then
            False
        else
            case ( getColliding components idA, getColliding components idB ) of
                ( Just a, Just b ) ->
                    checkAABB a.position a.size b.position b.size

                _ ->
                    False



{-
   resolveCollision : Time -> List Entity -> List Entity
   resolveCollision timeDelta entities =
       let
           makeCheckList =
               List.map (\x -> ( True, x )) entities

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
-}
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
    svg [ A.viewBox "0 0 20 15" ] <|
        showAllVisible model


showAllVisible : Visible a -> List (Svg msg)
showAllVisible components =
    let
        showVisible id =
            case getVisible components id of
                Just { position, size } ->
                    rect
                        [ A.x <| String.fromFloat <| Vec2.getX position
                        , A.y <| String.fromFloat <| Vec2.getY position
                        , A.width <| String.fromFloat <| Size.getWidth size
                        , A.height <| String.fromFloat <| Size.getHeight size
                        ]
                        []

                Nothing ->
                    svg [] []
    in
        List.map showVisible <| Dict.keys components.position



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- UTILITIES


type alias Time =
    Float



-- COMPONENTS


type alias Controllable a =
    { a
        | direction : Dict Int Vec2
        , controllable : Dict Int ()
    }


type alias Colliding a =
    { a
        | position : Dict Int Vec2
        , speed : Dict Int Float
        , direction : Dict Int Vec2
        , size : Dict Int Size
    }


type alias Dynamic a =
    { a
        | position : Dict Int Vec2
        , speed : Dict Int Float
        , direction : Dict Int Vec2
    }


type alias Visible a =
    { a
        | position : Dict Int Vec2
        , size : Dict Int Size
    }


getDynamic : Dynamic a -> Int -> Maybe Entity.Dynamic
getDynamic components id =
    case
        ( Dict.get id components.position
        , Dict.get id components.speed
        , Dict.get id components.direction
        )
    of
        ( Just position, Just speed, Just direction ) ->
            Just <|
                { position = position
                , speed = speed
                , direction = direction
                }

        _ ->
            Nothing


getVisible : Visible a -> Int -> Maybe Entity.Visible
getVisible components id =
    case
        ( Dict.get id components.position
        , Dict.get id components.size
        )
    of
        ( Just position, Just size ) ->
            Just <|
                { position = position
                , size = size
                }

        _ ->
            Nothing


getColliding : Colliding a -> Int -> Maybe Entity.Colliding
getColliding components id =
    case
        ( Dict.get id components.position
        , Dict.get id components.size
        )
    of
        ( Just position, Just size ) ->
            Just <|
                { position = position
                , size = size
                }

        _ ->
            Nothing



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
