module Lib.Utils exposing (..)

type alias Point2D =
    { x : Float
    , y : Float
    }

type alias Line2D =
    { xCof : Float
    , yCof : Float
    , rhs : Float
    }

type alias Plane3D =
    { xCof : Float
    , yCof : Float
    , zCof : Float
    , rhs : Float
    }

type alias Plane3DStyle =
    { color : Color
    , hasHatch : Bool
    }

type alias Point3D =
    { x : Float
    , y : Float
    , z : Float
    }

scale3D : Float -> Point3D -> Point3D
scale3D a p = Point3D (a * p.x) (a * p.y) (a * p.z)

add3D : Point3D -> Point3D -> Point3D
add3D p1 p2 = Point3D (p1.x + p2.x) (p1.y + p2.y) (p1.z + p2.z)

epsilon : Float
epsilon =  0.000000001

isZero : Float -> Bool
isZero x = abs x < epsilon

xValue : Plane3D -> Float -> Float -> Float
xValue pln y z =
    (pln.rhs - pln.yCof * y - pln.zCof * z)
    / pln.xCof

yValue : Plane3D -> Float -> Float -> Float
yValue pln x z =
    (pln.rhs - pln.xCof * x - pln.zCof * z)
    / pln.yCof

zValue : Plane3D -> Float -> Float -> Float
zValue pln x y =
    (pln.rhs - pln.xCof * x - pln.yCof * y)
    / pln.zCof

type alias Point3DStyle =
    { color : Color
    , hasGuides : Bool
    }

type Color
    = Red
    | Blue
    | Green
    | Black
    | Grey

stringFromColor : Color -> String
stringFromColor c =
    case c of
        Red -> "red"
        Blue -> "blue"
        Green -> "green"
        Black -> "black"
        Grey -> "grey"
