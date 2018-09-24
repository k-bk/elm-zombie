module Component
    exposing
        ( Entity
        , Component(..)
        , position
        , speed
        , direction
        , size
        , controllable
        , getComponent
        , updateComponent
        )

import Debug
import Html
import List
import Vector2 as Vec2 exposing (Vec2(..))


-- ECS documentation:
--   * Adding new component:
--      1. Update Component type:
--         type Component
--             = Position Vec2
--             | ..
--             | YourComponent Int
--      2. Add constructor for default value:
--
--         yourComponent : Component
--         yourComponent =
--             YourComponent 0
--
--      3. Update hasComponent / checkComoponent:
--
--         checkComponent with =
--             case ( component, with ) of
--                 .. ->
--                     ..
--
--                 ( YourComponent _, YourComponent ) ->
--                     True
--
--                 _ ->
--                     False
--
--      4. Have fun!


type alias Entity =
    List Component


type Component
    = Position Vec2
    | Speed Float
    | Direction Vec2
    | Size ( Float, Float )
    | Controllable


position : Component
position =
    Position Vec2.null


speed : Component
speed =
    Speed 0.0


direction : Component
direction =
    Direction Vec2.null


size : Component
size =
    Size ( 0, 0 )


controllable : Component
controllable =
    Controllable


isOfType : Component -> Component -> Bool
isOfType type1 type2 =
    case ( type1, type2 ) of
        ( Position _, Position _ ) ->
            True

        ( Speed _, Speed _ ) ->
            True

        ( Direction _, Direction _ ) ->
            True

        ( Size _, Size _ ) ->
            True

        ( Controllable, Controllable ) ->
            True

        _ ->
            False


updateComponent : Component -> Entity -> Component -> Entity
updateComponent componentType entity newValue =
    case entity of
        component :: rest ->
            if isOfType componentType component then
                newValue :: rest
            else
                component :: (updateComponent componentType rest newValue)

        [] ->
            []


getComponent : Component -> Entity -> Maybe Component
getComponent componentType entity =
    List.head <| List.filter (isOfType componentType) entity



-- TESTING FUNCTIONS


testList : List Entity
testList =
    [ [ Position Vec2.null ]
    , [ Position (Vec2 1 2) ]
    , [ Position (Vec2 2 3), Speed 2 ]
    , [ Speed 5 ]
    , [ Position (Vec2 33 3) ]
    ]


main =
    Html.text "Component test"
