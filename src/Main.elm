module Main exposing (..)

import Browser

import Html exposing (Html, h1, p, button, div, text, input)
import Html.Attributes as H exposing (..)
import Html.Events exposing (onClick, onInput)
import Lib.Utils exposing (..)
import Lib.Space exposing (basic3DHtml, Element(..))
import Time

main =
  Browser.document
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
  case msg of
    UpdateXCof xCof ->
      ( { model | xCof = xCof }, Cmd.none )
    UpdateYCof yCof ->
      ( { model | yCof = yCof }, Cmd.none )
    UpdateZCof zCof ->
      ( { model | zCof = zCof }, Cmd.none )
    UpdateRhs rhs ->
      ( { model | rhs = rhs }, Cmd.none )
    Tick _ ->
        let newRot = if model.rotation < 100 then model.rotation + 0.01 else 0 in
        ( { model | rotation = newRot}, Cmd.none )

view : Model -> Browser.Document Msg
view model =
    let title = "Linear Equations in ℝ³" in
    { title = title
    , body =
      [ div
        [ style "font-family" "Arial, sans-serif"
        , style "margin" "50px auto"
        , style "max-width" "600px"
        ]
        [ h1
          [ style "text-align" "center" ]
          [ text title ]
        , basic3DHtml
          { distance = 60
          , tilt = 0.2
          , rotation = model.rotation
          , viewBoxWidth = 600
          , quadrantWidth = 40
          , sceneWidth = 80
          , strokeWidth = 1
          }
          (
            [ Plane
              { xCof = model.xCof
              , yCof = model.yCof
              , zCof = model.zCof
              , rhs = model.rhs
              }
              { color = Red
              , hasHatch = True
              }
            ] ++
            (
              if model.xCof == 0.0 then [] else
              [ Point
                { x = model.rhs / model.xCof
                , y = 0.0
                , z = 0.0
                }
                { color = Grey
                , hasGuides = False
                }
              ]
            ) ++
            (
              if model.yCof == 0.0 then [] else
              [ Point
                { x = 0.0
                , y = model.rhs / model.yCof
                , z = 0.0
                }
                { color = Grey
                , hasGuides = False
                }
              ]
            ) ++
            (
              if model.zCof == 0.0 then [] else
              [ Point
                { x = 0.0
                , y = 0.0
                , z = model.rhs / model.zCof
                }
                { color = Grey
                , hasGuides = False
                }
              ]
            )
          )
        , div
          [ style "text-align" "center"
          , style "margin-top" "5px"
          , style "margin-bottom" "5px"
          ]
          [ input
            [ type_ "number"
            , H.min "-5.0"
            , H.max "5.0"
            , step "0.1"
            , value <| (String.fromFloat model.xCof)
            , onInput (\s -> UpdateXCof (Maybe.withDefault model.xCof (String.toFloat s)))
            ]
            []
          , text "x + "
          , input
            [ type_ "number"
            , H.min "-5.0"
            , H.max "5.0"
            , step "0.1"
            , value <| (String.fromFloat model.yCof)
            , onInput (\s -> UpdateYCof (Maybe.withDefault model.yCof (String.toFloat s)))
            ]
            []
          , text "y + "
          , input
            [ type_ "number"
            , H.min "0.5"
            , H.max "5.0"
            , step "0.1"
            , value <| (String.fromFloat model.zCof)
            , onInput (\s -> UpdateZCof (Maybe.withDefault model.zCof (String.toFloat s)))
            ]
            []
          , text "z = "
          , input
            [ type_ "number"
            , H.min "-5.0"
            , H.max "5.0"
            , step "0.1"
            , value <| (String.fromFloat model.rhs)
            , onInput (\s -> UpdateRhs (Maybe.withDefault model.rhs (String.toFloat s)))
            ]
            []
          ]
        , p []
          [
            text
            """
            A linear equation in ℝ³ is an equation over three
            variables that can be expressed in the above form.  The
            point set such an equation is a plane in ℝ³.
            """
          ]
        ]
      ]
    }
