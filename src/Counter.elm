module Counter exposing (..)

import Html exposing (Html, button, p, text)
import Html.Events exposing (onClick)
import Time exposing (Time, second)


-- MODEL


type alias Model =
    { count : Int
    , waitingIncrementAsync : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( Model 0 False, Cmd.none )



-- UPDATE


type Msg
    = Increment
    | IncrementIfOdd
    | IncrementAsync
    | IncrementAsyncComplete Time
    | Decrement


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( { model | count = model.count + 1 }, Cmd.none )

        IncrementIfOdd ->
            let
                count =
                    if model.count % 2 /= 0 then
                        model.count + 1
                    else
                        model.count
            in
                ( { model | count = count }, Cmd.none )

        IncrementAsync ->
            ( { model | waitingIncrementAsync = True }, Cmd.none )

        IncrementAsyncComplete _ ->
            ( Model (model.count + 1) False, Cmd.none )

        Decrement ->
            ( { model | count = model.count - 1 }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.waitingIncrementAsync then
        Time.every second IncrementAsyncComplete
    else
        Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    p []
        [ text ("Clicked: " ++ toString model.count ++ " times")
        , text " "
        , button [ onClick Decrement ] [ text "-" ]
        , text " "
        , button [ onClick Increment ] [ text "+" ]
        , text " "
        , button [ onClick IncrementIfOdd ] [ text "Increment if odd" ]
        , text " "
        , button [ onClick IncrementAsync ] [ text "Increment async" ]
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
