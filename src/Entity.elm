module Entity exposing (..)

import Size exposing (Size(..))
import Vector2 as Vec2 exposing (Vec2(..))


type alias Controllable =
    { direction : Vec2
    , controllable : ()
    }


type alias Colliding =
    { position : Vec2
    , size : Size
    }


type alias Dynamic =
    { position : Vec2
    , speed : Float
    , direction : Vec2
    }


type alias Visible =
    { position : Vec2
    , size : Size
    }
