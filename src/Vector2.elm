module Vector2
    exposing
        ( Vec2(..)
        , map
        , map2
        , toString
        , add
        , sub
        , scale
        )

-- VECTOR2


type Vec2
    = Vec2 Float Float


add : Vec2 -> Vec2 -> Vec2
add =
    map2 (+)


sub : Vec2 -> Vec2 -> Vec2
sub =
    map2 (-)


scale : Float -> Vec2 -> Vec2
scale x =
    map ((*) x)


dot : Vec2 -> Vec2 -> Float
dot (Vec2 x1 y1) (Vec2 x2 y2) =
    x1 * x2 + y1 * y2


neg : Vec2 -> Vec2
neg =
    map (\x -> -x)


round : Vec2 -> ( Int, Int )
round (Vec2 x y) =
    ( Basics.round x, Basics.round y )


map : (Float -> Float) -> Vec2 -> Vec2
map f (Vec2 x y) =
    Vec2 (f x) (f y)


map2 : (Float -> Float -> Float) -> Vec2 -> Vec2 -> Vec2
map2 f (Vec2 x1 y1) (Vec2 x2 y2) =
    Vec2 (f x1 x2) (f y1 y2)


toString : Vec2 -> String
toString (Vec2 x y) =
    "( " ++ String.fromFloat x ++ ", " ++ String.fromFloat y ++ " )"
