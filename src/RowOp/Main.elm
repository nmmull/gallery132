module RowOp.Main exposing (..)

import Browser

import Html exposing (Html, h1, p, button, div, text, input, pre, math)
import Html.Attributes as H exposing (..)
import Html.Events exposing (onClick, onInput)
import Lib.Utils exposing (..)
import Lib.Space exposing (scene3D, Element(..))
import Time
import Round exposing (round)

main = Browser.document
    { init = init
    , subscriptions = subscriptions
    , update = update
    , view = view
    }

type Msg
    = UpdateCof Float
    | Tick Time.Posix

type alias Model =
    { cof : Float
    , rotation : Float
    }

init : () -> ( Model, Cmd Msg )
init () =
    ( { cof = 0.0
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
            UpdateCof cof -> { model | cof = cof }
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
        title = "Row Replacements Preserve Solutions"
        sceneWidth = 15
        quadrantWidth = 5
        viewBoxWidth = 600
        interceptSty =
            { color = Grey
            , hasGuides = False
            }
        p1 =
            { xCof = 1
            , yCof = 2
            , zCof = 3
            , rhs = 4
            }
        p2 =
            { xCof = -1 + model.cof * 1
            , yCof = -2 + model.cof * 2
            , zCof = 1 + model.cof * 3
            , rhs = 0 + model.cof * 4
            }
        scene = List.concat
            [ [ Plane
                p1
                { color = Red
                , hasHatch = False
                }
              , Plane
                p2
                { color = Blue
                , hasHatch = False
                }
              , Intersection p1 p2
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
                , style "margin-top" "10px"
                , style "margin-bottom" "10px"
                ]
                [ pre
                    [ style "font-family" "monospace"
                    , style "margin-bottom" "10px"
                    ]
                    [ text
                        (
                            let
                                a = String.padLeft 5 ' ' (Round.round 1 (1 + model.cof * 1))
                                b = String.padLeft 5 ' ' (Round.round 1 (-2 + model.cof * 2))
                                c = String.padLeft 5 ' ' (Round.round 1 (1 + model.cof * 3))
                                d = String.padLeft 5 ' ' (Round.round 1 (0 + model.cof * 4))
                            in
                            String.join "\n"
                            [ "x₁ + 2x₂ + 3x₃ = 4"
                            , "x₁ - 2x₂ +  x₃ = 0"
                            , ""
                            , "┌                         ┐"
                            , "│   1.0   2.0   3.0   4.0 │"
                            , "│ " ++ a ++ " " ++ b ++ " " ++ c ++ " " ++ d ++ " │"
                            , "└                         ┘"
                            ]
                        )
                    ]
                , text "R₂ ⟵ R₂ + "
                , inputNum -5.0 5.0 0.1 model.cof
                    (
                        String.toFloat
                        >> Maybe.withDefault model.cof
                        >> UpdateCof
                    )
                , text "R₁"
                ]
            , p []
                [ text
                  """
                  The above is a linear system with two equations,
                  along with its augmented matrix after having applied
                  the given row replacement operation.  Notice that
                  the intersection of the representative planes
                  doesn't change, even though the second plane changes
                  as a result of the row replacement operation.
                  """
                ]
            ]
        ]
    }
