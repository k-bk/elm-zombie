module Vector2 exposing (..)

-- VECTOR2


type Vec2
    = Vec2 Float Float


null : Vec2
null =
    Vec2 0 0


normalize : Vec2 -> Vec2
normalize vector =
    if length vector < 0.001 then
        null
    else
        scale (1 / length vector) vector


getX : Vec2 -> Float
getX (Vec2 x _) =
    x


getY : Vec2 -> Float
getY (Vec2 _ y) =
    y


length : Vec2 -> Float
length (Vec2 x y) =
    sqrt <| x * x + y * y


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


negate : Vec2 -> Vec2
negate =
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
