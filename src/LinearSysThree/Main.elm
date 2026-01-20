module LinearSysThree.Main exposing (..)

import Browser

import Html exposing (Html, h1, p, button, div, text, input)
import Html.Attributes as H exposing (..)
import Html.Events exposing (onClick, onInput)
import Lib.Utils exposing (..)
import Lib.Space exposing (scene3D, Element(..))
import Time

main = Browser.document
    { init = init
    , subscriptions = subscriptions
    , update = update
    , view = view
    }

type Msg
    = UpdateXCof1 Float
    | UpdateYCof1 Float
    | UpdateZCof1 Float
    | UpdateRhs1 Float
    | UpdateXCof2 Float
    | UpdateYCof2 Float
    | UpdateZCof2 Float
    | UpdateRhs2 Float
    | UpdateXCof3 Float
    | UpdateYCof3 Float
    | UpdateZCof3 Float
    | UpdateRhs3 Float
    | Tick Time.Posix

type alias Model =
    { xCof1 : Float
    , yCof1 : Float
    , zCof1 : Float
    , rhs1 : Float
    , xCof2 : Float
    , yCof2 : Float
    , zCof2 : Float
    , rhs2 : Float
    , xCof3 : Float
    , yCof3 : Float
    , zCof3 : Float
    , rhs3 : Float
    , rotation : Float
    }

init : () -> ( Model, Cmd Msg )
init () =
    ( { xCof1 = 1.0
      , yCof1 = 2.0
      , zCof1 = 3.0
      , rhs1 = 4.0
      , xCof2 = 2.0
      , yCof2 = 3.0
      , zCof2 = 4.0
      , rhs2 = 5.0
      , xCof3 = 1.0
      , yCof3 = 2.0
      , zCof3 = 3.0
      , rhs3 = -2.0
      , rotation = 0
      }
    , Cmd.none
    )

subscriptions : Model -> Sub Msg
subscriptions model = Time.every 50 Tick

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        update_ msg_ = case msg_ of
            UpdateXCof1 xCof1 -> { model | xCof1 = xCof1 }
            UpdateYCof1 yCof1 -> { model | yCof1 = yCof1 }
            UpdateZCof1 zCof1 -> { model | zCof1 = zCof1 }
            UpdateRhs1 rhs1 -> { model | rhs1 = rhs1 }
            UpdateXCof2 xCof2 -> { model | xCof2 = xCof2 }
            UpdateYCof2 yCof2 -> { model | yCof2 = yCof2 }
            UpdateZCof2 zCof2 -> { model | zCof2 = zCof2 }
            UpdateRhs2 rhs2 -> { model | rhs2 = rhs2 }
            UpdateXCof3 xCof3 -> { model | xCof3 = xCof3 }
            UpdateYCof3 yCof3 -> { model | yCof3 = yCof3 }
            UpdateZCof3 zCof3 -> { model | zCof3 = zCof3 }
            UpdateRhs3 rhs3 -> { model | rhs3 = rhs3 }
            Tick _ ->
                let
                    newRot =
                        if model.rotation < 10000
                        then model.rotation + 0.01
                        else 0
                in
                { model | rotation = newRot}
    in ( update_ msg, Cmd.none )

view : Model -> Browser.Document Msg
view model =
    let
        title = "Linear System in Three Variables"
        sceneWidth = 15
        quadrantWidth = 5
        viewBoxWidth = 600
        interceptSty =
            { color = Grey
            , hasGuides = False
            }
        p1 =
            { xCof = model.xCof1
            , yCof = model.yCof1
            , zCof = model.zCof1
            , rhs = model.rhs1
            }
        p2 =
            { xCof = model.xCof2
            , yCof = model.yCof2
            , zCof = model.zCof2
            , rhs = model.rhs2
            }
        p3 =
            { xCof = model.xCof3
            , yCof = model.yCof3
            , zCof = model.zCof3
            , rhs = model.rhs3
            }
        scene = List.concat
            [ [ Plane
                p1
                { color = Blue
                , hasHatch = False
                }
              , Plane
                p2
                { color = Green
                , hasHatch = False
                }
              , Plane
                p3
                { color = Red
                , hasHatch = False
                }
              , Intersection p1 p2
              , Intersection p2 p3
              , Intersection p1 p3
              ]
            ]
        inputNum mn mx stp val onIn = input
            [ type_ "number"
            , H.min (String.fromFloat mn)
            , H.max (String.fromFloat mx)
            , step (String.fromFloat stp)
            , value <| (String.fromFloat val)
            , onInput onIn
            ]
            []
    in
    { title = title
    , body =
        [ div
            [ style "font-family" "Arial, sans-serif"
            , style "margin" "50px auto"
            , style "max-width" (String.fromInt viewBoxWidth ++ "px")
            ]
            [ h1
                [ style "text-align" "center" ]
                [ text title ]
            , scene3D
                { distance = 50
                , tilt = 0.2
                , strokeWidth = 1
                , rotation = model.rotation
                , viewBoxWidth = viewBoxWidth
                , quadrantWidth = quadrantWidth
                , sceneWidth = sceneWidth
                }
                scene
            , div
                [ style "text-align" "center"
                , style "margin-top" "5px"
                , style "margin-bottom" "5px"
                ]
                [ inputNum -5.0 5.0 0.1 model.xCof1
                    (
                        String.toFloat
                        >> Maybe.withDefault model.xCof1
                        >> UpdateXCof1
                    )
                , text "x + "
                , inputNum -5.0 5.0 0.1 model.yCof1
                    (
                        String.toFloat
                        >> Maybe.withDefault model.yCof1
                        >> UpdateYCof1
                    )
                , text "y + "
                , inputNum -5.0 5.0 0.1 model.zCof1
                    (
                        String.toFloat
                        >> Maybe.withDefault model.zCof1
                        >> UpdateZCof1
                    )
                , text "z = "
                , inputNum -10.0 10.0 0.1 model.rhs1
                    (
                        String.toFloat
                        >> Maybe.withDefault model.rhs1
                        >> UpdateRhs1
                    )
                ]
            , div
                [ style "text-align" "center"
                , style "margin-top" "5px"
                , style "margin-bottom" "5px"
                ]
                [ inputNum -5.0 5.0 0.1 model.xCof2
                    (
                        String.toFloat
                        >> Maybe.withDefault model.xCof2
                        >> UpdateXCof2
                    )
                , text "x + "
                , inputNum -5.0 5.0 0.1 model.yCof2
                    (
                        String.toFloat
                        >> Maybe.withDefault model.yCof2
                        >> UpdateYCof2
                    )
                , text "y + "
                , inputNum -5.0 5.0 0.1 model.zCof2
                    (
                        String.toFloat
                        >> Maybe.withDefault model.zCof2
                        >> UpdateZCof2
                    )
                , text "z = "
                , inputNum -10.0 10.0 0.1 model.rhs2
                    (
                        String.toFloat
                        >> Maybe.withDefault model.rhs2
                        >> UpdateRhs2
                    )
                ]
            , div
                [ style "text-align" "center"
                , style "margin-top" "5px"
                , style "margin-bottom" "5px"
                ]
                [ inputNum -5.0 5.0 0.1 model.xCof3
                    (
                        String.toFloat
                        >> Maybe.withDefault model.xCof3
                        >> UpdateXCof3
                    )
                , text "x + "
                , inputNum -5.0 5.0 0.1 model.yCof3
                    (
                        String.toFloat
                        >> Maybe.withDefault model.yCof3
                        >> UpdateYCof3
                    )
                , text "y + "
                , inputNum -5.0 5.0 0.1 model.zCof3
                    (
                        String.toFloat
                        >> Maybe.withDefault model.zCof3
                        >> UpdateZCof3
                    )
                , text "z = "
                , inputNum -10.0 10.0 0.1 model.rhs3
                    (
                        String.toFloat
                        >> Maybe.withDefault model.rhs3
                        >> UpdateRhs3
                    )
                ]
            ]
        ]
    }
