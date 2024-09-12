module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Random exposing (Generator, Seed, step, uniform)



-- MODEL


type alias Model =
    { x : Int
    , y : Int
    , direction : Direction
    , fontGroup : FontGroup
    }


type Direction
    = North
    | East
    | South
    | West


type FontGroup
    = GroupA -- Existing font
    | GroupB -- New font


init : () -> ( Model, Cmd Msg )
init _ =
    let
        seed =
            Random.initialSeed 12345

        ( randomGroup, _ ) =
            Random.step fontGroupGenerator seed
    in
    ( { x = 2, y = 2, direction = North, fontGroup = randomGroup }, Cmd.none )


fontGroupGenerator : Generator FontGroup
fontGroupGenerator =
    Random.uniform GroupA [ GroupB ]



-- MESSAGES


type Msg
    = MoveForward
    | RotateLeft
    | RotateRight



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MoveForward ->
            ( moveForward model, Cmd.none )

        RotateLeft ->
            ( { model | direction = rotateLeft model.direction }, Cmd.none )

        RotateRight ->
            ( { model | direction = rotateRight model.direction }, Cmd.none )


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
    div [ class ("grid-container " ++ fontClass model.fontGroup) ]
        [ div [ class "grid" ]
            [ text (gridRepresentation model)
            ]
        , div [ class "controls" ]
            [ button [ onClick RotateLeft, class "button" ] [ text "Rotate Left" ]
            , button [ onClick MoveForward, class "button" ] [ text "Move Forward" ]
            , button [ onClick RotateRight, class "button" ] [ text "Rotate Right" ]
            ]
        ]


fontClass : FontGroup -> String
fontClass group =
    case group of
        GroupA ->
            "group-a-font"

        -- Existing font class
        GroupB ->
            "group-b-font"



-- New font you're testing


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
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }
