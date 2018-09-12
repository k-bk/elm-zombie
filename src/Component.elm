module Component exposing (Entity, Component, position, speed, hasComponent)

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
    ( Int, List Component )


type Component
    = Position Vec2
    | Speed Float


position : Component
position =
    Position Vec2.null


speed : Component
speed =
    Speed 0.0


hasComponent : Component -> Entity -> Bool
hasComponent component ( _, list ) =
    let
        checkComponent with =
            case ( component, with ) of
                ( Position _, Position _ ) ->
                    True

                ( Speed _, Speed _ ) ->
                    True

                _ ->
                    False
    in
        List.any checkComponent list



-- TESTING FUNCTIONS


testList : List Entity
testList =
    [ ( 1, [ Position Vec2.null ] )
    , ( 2, [ Position (Vec2 1 2) ] )
    , ( 3, [ Position (Vec2 2 3), Speed 2 ] )
    , ( 4, [ Speed 5 ] )
    , ( 5, [ Position (Vec2 33 3) ] )
    ]


takeWithPosition =
    List.filter (hasComponent position) testList


takeWithSpeed =
    List.filter (hasComponent speed) testList
