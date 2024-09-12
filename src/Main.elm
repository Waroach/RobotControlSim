module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)



-- MODEL


type alias Model =
    { x : Int
    , y : Int
    , direction : Direction
    }


type Direction
    = North
    | East
    | South
    | West


init : Model
init =
    { x = 2, y = 2, direction = North }



-- UPDATE


type Msg
    = MoveForward
    | RotateLeft
    | RotateRight


update : Msg -> Model -> Model
update msg model =
    case msg of
        MoveForward ->
            moveForward model

        RotateLeft ->
            { model | direction = rotateLeft model.direction }

        RotateRight ->
            { model | direction = rotateRight model.direction }


moveForward : Model -> Model
moveForward model =
    case model.direction of
        North ->
            if model.y > 0 then
                { model | y = model.y - 1 }

            else
                model

        South ->
            if model.y < 4 then
                { model | y = model.y + 1 }

            else
                model

        East ->
            if model.x < 4 then
                { model | x = model.x + 1 }

            else
                model

        West ->
            if model.x > 0 then
                { model | x = model.x - 1 }

            else
                model


rotateLeft : Direction -> Direction
rotateLeft direction =
    case direction of
        North ->
            West

        West ->
            South

        South ->
            East

        East ->
            North


rotateRight : Direction -> Direction
rotateRight direction =
    case direction of
        North ->
            East

        East ->
            South

        South ->
            West

        West ->
            North



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "grid-container" ]
        [ div [ class "grid" ]
            [ text (gridRepresentation model)
            ]
        , div [ class "controls" ]
            [ button [ onClick RotateLeft, class "button" ] [ text "Rotate Left" ]
            , button [ onClick MoveForward, class "button" ] [ text "Move Forward" ]
            , button [ onClick RotateRight, class "button" ] [ text "Rotate Right" ]
            ]
        ]


gridRepresentation : Model -> String
gridRepresentation model =
    "Robot is at (" ++ String.fromInt model.x ++ ", " ++ String.fromInt model.y ++ ") facing " ++ directionToString model.direction


directionToString : Direction -> String
directionToString direction =
    case direction of
        North ->
            "North"

        East ->
            "East"

        South ->
            "South"

        West ->
            "West"



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }
