module LinearEqThree.Main exposing (..)

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
    = UpdateXCof Float
    | UpdateYCof Float
    | UpdateZCof Float
    | UpdateRhs Float
    | Tick Time.Posix

type alias Model =
    { xCof : Float
    , yCof : Float
    , zCof : Float
    , rhs : Float
    , rotation : Float
    }

init : () -> ( Model, Cmd Msg )
init () =
    ( { xCof = 1.0
      , yCof = 2.0
      , zCof = 3.0
      , rhs = 4.0
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
            UpdateXCof xCof -> { model | xCof = xCof }
            UpdateYCof yCof -> { model | yCof = yCof }
            UpdateZCof zCof -> { model | zCof = zCof }
            UpdateRhs rhs -> { model | rhs = rhs }
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
        title = "Linear Equations in Three Variables"
        sceneWidth = 15
        quadrantWidth = 5
        viewBoxWidth = 600
        interceptSty =
            { color = Grey
            , hasGuides = False
            }
        scene = List.concat
            [ [ Plane
                { xCof = model.xCof
                , yCof = model.yCof
                , zCof = model.zCof
                , rhs = model.rhs
                }
                { color = Red
                , hasHatch = True
                }
              ]
            , let x = model.rhs / model.xCof in
              if model.xCof == 0.0 || abs x > toFloat quadrantWidth
              then []
              else [ Point { x = x, y = 0.0, z = 0.0 } interceptSty ]
            , let y = model.rhs / model.yCof in
              if model.yCof == 0.0 || abs y > toFloat quadrantWidth
              then []
              else [ Point { x = 0.0, y = y, z = 0.0} interceptSty ]
            , let z = model.rhs / model.zCof in
              if model.zCof == 0.0 || abs z > toFloat quadrantWidth
              then []
              else [ Point { x = 0.0, y = 0.0, z = z } interceptSty ]
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
                [ inputNum -5.0 5.0 0.1 model.xCof
                    (
                        String.toFloat
                        >> Maybe.withDefault model.xCof
                        >> UpdateXCof
                    )
                , text "x + "
                , inputNum -5.0 5.0 0.1 model.yCof
                    (
                        String.toFloat
                        >> Maybe.withDefault model.yCof
                        >> UpdateYCof
                    )
                , text "y + "
                , inputNum -5.0 5.0 0.1 model.zCof
                    (
                        String.toFloat
                        >> Maybe.withDefault model.zCof
                        >> UpdateZCof
                    )
                , text "z = "
                , inputNum -10.0 10.0 0.1 model.rhs
                    (
                        String.toFloat
                        >> Maybe.withDefault model.rhs
                        >> UpdateRhs
                    )
                ]
            , p []
                [ text
                  """
                  A linear equation in three variables can be
                  expressed in the above form.  The point set such an
                  equation is a plane in ℝ³.
                  """
                ]
            ]
        ]
    }
