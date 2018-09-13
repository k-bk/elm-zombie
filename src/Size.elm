module Size exposing (..)


type Size
    = Size Float Float


getWidth (Size width height) =
    width


getHeight (Size width height) =
    height
