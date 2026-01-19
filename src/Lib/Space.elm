module Lib.Space exposing (..)

import Lib.Utils exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html exposing (Html, div)

import Browser exposing (element)
import Time

type alias Params a =
    { a
    | distance : Int
    , tilt : Float
    , rotation : Float
    , quadrantWidth : Int
    , viewBoxWidth : Int
    , sceneWidth : Int
    , strokeWidth : Float
    }

minC : Params a -> Float
minC params = -(toFloat params.quadrantWidth)

maxC : Params a -> Float
maxC params = toFloat params.quadrantWidth

rotZ : Params a -> Point3D -> Point3D
rotZ params pt =
    let theta = params.rotation in
    Point3D
        (cos theta * pt.x - sin theta * pt.y)
        (sin theta * pt.x + cos theta * pt.y)
        pt.z

rotY : Params a -> Point3D -> Point3D
rotY params pt =
    let theta = params.tilt in
    Point3D
        (cos theta * pt.x + sin theta * pt.z)
        pt.y
        (-(sin theta) * pt.x + cos theta * pt.z)

perspective : Params a -> Point3D -> Point2D
perspective params pt =
    let d = toFloat params.distance in
    Point2D
        (pt.y / (1 - pt.x / d))
        (pt.z / (1 - pt.x / d))

shift : Params a -> Point3D -> Point2D
shift params pt =
    let
        w = toFloat params.viewBoxWidth
        space = w / toFloat params.sceneWidth
        offset = space * toFloat params.sceneWidth / 2
        drawPt = perspective params (rotY params (rotZ params pt))
    in
    Point2D
        (drawPt.x * space + offset)
        (-drawPt.y * space + offset)

myLine :
    Params a
    -> Point3D
    -> Point3D
    -> List (Attribute msg)
    -> Svg msg
myLine params p1 p2 baseAttrs =
    let
        str = String.fromFloat
        start = shift params p1
        end = shift params p2
        endpoints =
            [ x1 (str start.x)
            , y1 (str start.y)
            , x2 (str end.x)
            , y2 (str end.y)
            ]
    in line (endpoints ++ baseAttrs) []

myCircle : Params a -> Point3D -> List (Attribute msg) -> Svg msg
myCircle params pt baseAttrs =
    let
        center = shift params pt
        str = String.fromFloat
        attr =
            [ cx (str center.x)
            , cy (str center.y)
            ]
    in circle (attr ++ baseAttrs) []

drawAxis : Params a -> List (Svg msg)
drawAxis params =
    let
        str = String.fromFloat
        baseAttrs =
            [ stroke "black"
            , strokeWidth (str params.strokeWidth)
            ]
        axis p1 p2 = myLine params p1 p2 baseAttrs
        mnC = minC params
        mxC = maxC params
    in
    [ axis (Point3D mnC 0 0) (Point3D mxC 0 0)
    , axis (Point3D 0 mnC 0) (Point3D 0 mxC 0)
    , axis (Point3D 0 0 mnC) (Point3D 0 0 mxC)
    , myCircle params (Point3D mxC 0 0)
        [ r (str (params.strokeWidth * 2))
        , fill "black"
        ]
    ]

myPolygon : Params a -> List Point3D -> List (Attribute msg) -> Svg msg
myPolygon params pts baseAttrs =
    let
        drawPts = List.map (shift params) pts
        str = String.fromFloat
        toStr p = str p.x ++ "," ++ str p.y
        strPts = String.join " " (List.map toStr drawPts)
    in polygon (points strPts :: baseAttrs) []

drawPoint : Params a -> Point3D -> Point3DStyle -> List (Svg msg)
drawPoint params pt sty =
    let
        rStr = String.fromFloat (params.strokeWidth * 2)
        guide p1 p2 = myLine params p1 p2
            [ stroke "grey"
            , strokeDasharray rStr
            , strokeWidth (String.fromFloat (params.strokeWidth / 2))
            ]
        guides =
            [ guide (Point3D pt.x 0 0) (Point3D pt.x pt.y 0)
            , guide (Point3D pt.x pt.y 0) pt
            , guide (Point3D 0 pt.y 0) (Point3D pt.x pt.y 0)
            , guide (Point3D 0 0 pt.z) (Point3D 0 pt.y pt.z)
            , guide (Point3D 0 pt.y pt.z) pt
            , guide (Point3D pt.x 0 pt.z) pt
            , guide (Point3D 0 0 pt.z) (Point3D pt.x 0 pt.z)
            , guide (Point3D pt.x 0 0) (Point3D pt.x 0 pt.z)
            , guide (Point3D 0 pt.y 0) (Point3D 0 pt.y pt.z)
            ]
    in
    List.concat
        [ if sty.hasGuides then guides else []
        , [ myCircle params pt
            [ fill (stringFromColor sty.color)
            , r rStr
            ]
          ]
        ]

inBounds : Params a -> Float -> Bool
inBounds params x =
    x > minC params - epsilon
    && x < maxC params + epsilon

clampLine :
    Params a
    -> Float
    -> Float
    -> Point3D
    -> Point3D
    -> List (Attribute msg)
    -> Svg msg
clampLine params l r p1 p2 sty =
    let
        a1 = (clamp l r p1.z - p2.z) / (p1.z - p2.z)
        a2 = (clamp l r p2.z - p1.z) / (p2.z - p1.z)
    in
    myLine
        params
        (add3D (scale3D a1 p1) (scale3D (1 - a1) p2))
        (add3D (scale3D a2 p2) (scale3D (1 - a2) p1))
        sty

drawPlane : Params a -> Plane3D -> Plane3DStyle -> List (Svg msg)
drawPlane params pln sty =
    let
        xVal = xValue pln
        yVal = yValue pln
        zVal = zValue pln
        mnC = minC params
        mxC = maxC params
        mnZ = 2 * mnC
        mxZ = 2 * mxC
        bottomLeft =
            let candZ = zVal mnC mnC in
            if abs candZ <= mxZ
            then [ Point3D mnC mnC candZ ]
            else
                let
                    z = clamp mnZ mxZ candZ
                    x = xVal mnC z
                    y = yVal mnC z
                in
                List.concat
                    [ if abs x > mxC then [] else [ Point3D x mnC z ]
                    , if abs y > mxC then [] else [ Point3D mnC y z ]
                    ]
        topLeft =
            let candZ = zVal mnC mxC in
            if abs candZ <= mxZ
            then [ Point3D mnC mxC candZ ]
            else
                let
                    z = clamp mnZ mxZ candZ
                    y = yVal mnC z
                    x = xVal mxC z
                in
                List.concat
                    [ if abs y > mxC then [] else [ Point3D mnC y z ]
                    , if abs x > mxC then [] else [ Point3D x mxC z ]
                    ]
        topRight =
            let candZ = zVal mxC mxC in
            if abs candZ <= mxZ
            then [ Point3D mxC mxC candZ ]
            else
                let
                    z = clamp mnZ mxZ candZ
                    x = xVal mxC z
                    y = yVal mxC z
                in
                List.concat
                    [ if abs x > mxC then [] else [ Point3D x mxC z ]
                    , if abs y > mxC then [] else [ Point3D mxC y z ]
                    ]
        bottomRight =
            let candZ = zVal mxC mnC in
            if abs candZ <= mxZ
            then [ Point3D mxC mnC candZ ]
            else
                let
                    z = clamp mnZ mxZ candZ
                    y = yVal mxC z
                    x = xVal mnC z
                in
                List.concat
                    [ if abs y > mxC then [] else [ Point3D mxC y z ]
                    , if abs x > mxC then [] else [ Point3D x mnC z ]
                    ]
        pts = List.concat
            [ bottomLeft
            , topLeft
            , topRight
            , bottomRight
            ]
        poly = myPolygon
            params
            pts
            [ fill (stringFromColor sty.color)
            , opacity "0.27"
            , stroke "black"
            ]
        hatchStyle =
            [ strokeWidth (String.fromFloat params.strokeWidth)
            , stroke "black"
            , opacity "0.1"
            ]
        xline x = clampLine params mnZ mxZ
            (Point3D x mnC (zVal x mnC))
            (Point3D x mxC (zVal x mxC))
            hatchStyle
        xlines =
            List.map
                (\x -> xline (toFloat x))
                (List.range (ceiling mnC + 1) (floor mxC - 1))
        yline y = clampLine params mnZ mxZ
            (Point3D mnC y (zVal mnC y))
            (Point3D mxC y (zVal mxC y))
            hatchStyle
        ylines =
            List.map
                (\y -> yline (toFloat y))
                (List.range (ceiling mnC + 1) (floor mxC - 1))
    in
    List.concat
        [ [ poly ]
        , if sty.hasHatch then xlines ++ ylines else []
        ]

getInterByX : Params a -> Plane3D -> Plane3D -> Float -> Maybe Point3D
getInterByX params pln1 pln2 x =
    let
        a = ( pln1.xCof, pln2.xCof )
        b = ( pln1.yCof, pln2.yCof )
        c = ( pln1.zCof, pln2.zCof )
        d = ( pln1.rhs, pln2.rhs )
        det ( e1, e2 ) ( f1, f2 ) = e1 * f2 - f1 * e2
        mxC = maxC params / 2
        y = -(det c d + (det a c) * x)
        z = (det b d + (det a b) * x)
    in
        if not (isZero (det b c)) && abs (y / det b c) <= mxC + epsilon
        then Just (Point3D x (y / det b c) (z / det b c))
        else Nothing

getInterByY : Params a -> Plane3D -> Plane3D -> Float -> Maybe Point3D
getInterByY params pln1 pln2 yVal =
    let
        pln11 = Plane3D pln1.yCof pln1.xCof pln1.zCof pln1.rhs
        pln22 = Plane3D pln2.yCof pln2.xCof pln2.zCof pln2.rhs
        swap { x, y, z } = Point3D y x z
   in Maybe.map swap (getInterByX params pln11 pln22 yVal)

intersections : Params a -> Plane3D -> Plane3D -> List Point3D
intersections params pln1 pln2 =
    let
        toList m =
            case m of
                Nothing -> []
                Just a -> [a]
        mxC = maxC params / 2
        mnC = minC params / 2
        det = pln1.xCof * pln2.yCof - pln1.yCof * pln2.xCof
        checkV =
            isZero pln1.zCof
            && isZero pln2.zCof
            && not (isZero det)
    in
    if checkV then
        let
            xInt = (pln2.yCof * pln1.rhs - pln2.xCof * pln2.rhs) / det
            yInt = (pln1.xCof * pln2.rhs - pln2.xCof * pln1.rhs) / det
        in
        [ Point3D xInt yInt (mnC * 2.5)
        , Point3D xInt yInt (mxC * 2.5)
        ]
    else
        List.concatMap toList
            [ getInterByX params pln1 pln2 mnC
            , getInterByX params pln1 pln2 mxC
            , getInterByY params pln1 pln2 mnC
            , getInterByY params pln1 pln2 mxC
            ]

drawIntersection : Params a -> Plane3D -> Plane3D -> List (Svg msg)
drawIntersection params pln1 pln2 =
    case intersections params pln1 pln2 of
        x :: y :: [] ->
            [ myLine params x y
                [ stroke "grey"
                , strokeWidth (String.fromFloat params.strokeWidth)
                ]
            ]
        _ -> []

type Element
    = Plane Plane3D Plane3DStyle
    | Point Point3D Point3DStyle
    | Intersection Plane3D Plane3D

drawElement : Params a -> Element -> List (Svg msg)
drawElement params e =
    case e of
        Plane pln sty -> drawPlane params pln sty
        Point pt sty -> drawPoint params pt sty
        Intersection pln1 pln2 -> drawIntersection params pln1 pln2

drawScene : Params a -> List Element -> List (Svg msg)
drawScene params scene = List.concatMap (drawElement params) scene

scene3D : Params a -> List Element -> Html msg
scene3D params elems =
    let w = String.fromInt params.viewBoxWidth in
    svg
        [ width w
        , height w
        , Svg.Attributes.style "border:1px solid black"
        , viewBox ("0 0 " ++ w ++ " " ++ w)
        ]
        (drawAxis params ++ drawScene params elems)
