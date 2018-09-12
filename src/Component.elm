module Component
    exposing
        ( Entity
        , Component(..)
        , position
        , speed
        , movement
        , width
        , height
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
    ( Int, List Component )


type Component
    = Position Vec2
    | Speed Float
    | Movement Vec2
    | Width Float
    | Height Float
    | Controllable


position : Component
position =
    Position Vec2.null


speed : Component
speed =
    Speed 0.0


movement : Component
movement =
    Movement Vec2.null


width : Component
width =
    Width 0.0


height : Component
height =
    Height 0.0


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

        ( Movement _, Movement _ ) ->
            True

        ( Width _, Width _ ) ->
            True

        ( Height _, Height _ ) ->
            True

        ( Controllable, Controllable ) ->
            True

        _ ->
            False


updateComponent : Component -> Entity -> Component -> Entity
updateComponent componentType ( id, components ) newValue =
    let
        updateList list =
            case list of
                component :: rest ->
                    if isOfType componentType component then
                        newValue :: rest
                    else
                        component :: (updateList rest)

                [] ->
                    []
    in
        ( id, updateList components )


getComponent : Component -> Entity -> Maybe Component
getComponent componentType ( _, list ) =
    List.head <| List.filter (isOfType componentType) list



-- TESTING FUNCTIONS


testList : List Entity
testList =
    [ ( 1, [ Position Vec2.null ] )
    , ( 2, [ Position (Vec2 1 2) ] )
    , ( 3, [ Position (Vec2 2 3), Speed 2 ] )
    , ( 4, [ Speed 5 ] )
    , ( 5, [ Position (Vec2 33 3) ] )
    ]


main =
    Html.text "Component test"
