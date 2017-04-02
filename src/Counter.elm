module Counter exposing (..)

import Html exposing (Html, button, p, program, text)
import Html.Events exposing (onClick)
import Time exposing (second)
import Process exposing (sleep)
import Task exposing (andThen, perform, succeed)


-- MODEL


type alias Model =
    Int


init : ( Model, Cmd Msg )
init =
    ( 0, Cmd.none )



-- UPDATE


type Msg
    = Increment
    | IncrementIfOdd
    | IncrementAsync
    | Decrement


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( model + 1, Cmd.none )

        IncrementIfOdd ->
            let
                count =
                    if model % 2 /= 0 then
                        model + 1
                    else
                        model
            in
                ( count, Cmd.none )

        IncrementAsync ->
            ( model, incrementAsync )

        Decrement ->
            ( model - 1, Cmd.none )


incrementAsync : Cmd Msg
incrementAsync =
    perform identity (andThen (\_ -> succeed Increment) (sleep second))



-- VIEW


view : Model -> Html Msg
view model =
    p []
        [ text ("Clicked: " ++ toString model ++ " times")
        , text " "
        , button [ onClick Increment ] [ text "+" ]
        , text " "
        , button [ onClick Decrement ] [ text "-" ]
        , text " "
        , button [ onClick IncrementIfOdd ] [ text "Increment if odd" ]
        , text " "
        , button [ onClick IncrementAsync ] [ text "Increment async" ]
        ]



-- MAIN


main : Program Never Model Msg
main =
    program
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }
