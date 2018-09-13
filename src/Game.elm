module Game exposing (..)

import Browser
import Browser.Events exposing (Visibility)
import Debug
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Json.Decode as Decode
import List
import Size exposing (Size(..))
import Svg exposing (Svg, svg, rect)
import Svg.Attributes as A
import Vector2 as Vec2 exposing (Vec2(..))


-- ENTITIES


type alias Id =
    Int


type alias Time =
    Float


type alias Dynamic a =
    { a
        | position : Vec2
        , speed : Float
        , direction : Vec2
    }


type alias Colliding a =
    { a
        | position : Vec2
        , box : Size
    }


type alias Controllable a =
    { a
        | controllable : ()
        , direction : Vec2
    }


type alias Visible a =
    { a
        | position : Vec2
        , box : Size
    }


type alias Entity =
    { id : Int }



-- MODEL


type alias Model =
    { player : Dynamic (Colliding (Controllable (Visible Entity)))
    , enemies : List (Dynamic (Colliding (Visible Entity)))
    , obstacles : List (Colliding (Visible Entity))
    , timeDelta : Time
    , timeDeltaHistory : List Time
    , input : List Key
    }


initModel : Model
initModel =
    let
        player : Dynamic (Colliding (Controllable (Visible Entity)))
        player =
            { id = 1
            , position = Vec2 2 4
            , speed = 5
            , direction = Vec2.null
            , box = Size 1 2
            , controllable = ()
            }

        flyingBird : Dynamic (Colliding (Visible Entity))
        flyingBird =
            { id = 2
            , position = Vec2 2 4
            , speed = 1
            , direction = Vec2 1 0
            , box = Size 2 1
            }

        block : Float -> Float -> Int -> Colliding (Visible Entity)
        block x y id =
            { id = id
            , position = Vec2 x y
            , box = Size 2 2
            }
    in
        { player = player
        , enemies = [ flyingBird ]
        , obstacles = [ block 2 2 3, block 2 5 4, block 5 5 5 ]
        , timeDelta = 0
        , timeDeltaHistory = []
        , input = []
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
            case msg of
                Tick timeDelta ->
                    -- update time sync things
                    model
                        |> updateTime msg
                        |> updateAllControllable msg
                        |> updateAllDynamic msg
                        |> updateAllColliding msg

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


updateTime : Msg -> Model -> Model
updateTime msg model =
    case msg of
        Tick timeDelta ->
            { model
                | timeDelta = timeDelta
                , timeDeltaHistory =
                    List.take 10
                        (timeDelta :: model.timeDeltaHistory)
            }

        _ ->
            model


updateAllControllable : Msg -> Model -> Model
updateAllControllable msg ({ player } as model) =
    let
        newDirection =
            keysToVector model.input
                |> Vec2.normalize
    in
        { model | player = { player | direction = newDirection } }


updateAllDynamic : Msg -> Model -> Model
updateAllDynamic msg ({ player, enemies, timeDelta } as model) =
    { model
        | player = (updateDynamic timeDelta) player
        , enemies = List.map (updateDynamic timeDelta) enemies
    }


updateDynamic : Time -> Dynamic a -> Dynamic a
updateDynamic timeDelta ({ position, speed, direction } as entity) =
    let
        newPosition =
            direction
                |> Vec2.normalize
                |> Vec2.scale speed
                |> Vec2.scale timeDelta
                |> Vec2.add position
    in
        { entity | position = newPosition }


updateAllColliding : Msg -> Model -> Model
updateAllColliding msg ({ player, enemies, obstacles, timeDelta } as model) =
    model



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


isColliding : Colliding a -> Colliding a -> Bool
isColliding a b =
    if a == b then
        False
    else
        (Vec2.getX a.position < Vec2.getX b.position + Size.getWidth b.box)
            && (Vec2.getX a.position + Size.getWidth a.box > Vec2.getX b.position)
            && (Vec2.getY a.position < Vec2.getY b.position + Size.getHeight b.box)
            && (Vec2.getY a.position + Size.getHeight a.box > Vec2.getY b.position)



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
        [ viewVisible model.player ]
            ++ (List.map viewVisible model.enemies)
            ++ (List.map viewVisible model.obstacles)


viewVisible : Visible a -> Svg msg
viewVisible { position, box } =
    rect
        [ A.x <| String.fromFloat <| Vec2.getX position
        , A.y <| String.fromFloat <| Vec2.getY position
        , A.width <| String.fromFloat <| Size.getWidth box
        , A.height <| String.fromFloat <| Size.getHeight box
        ]
        []



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
