module Tests exposing (..)

import Test exposing (..)
import Expect exposing (equal)
import Fuzz exposing (int)
import Counter exposing (..)
import Test.Html.Query exposing (fromHtml, has)
import Test.Html.Selector exposing (text)


all : Test
all =
    describe "Counter Test Suite"
        [ test "init" <|
            \() ->
                init |> equal ( 0, Cmd.none )
        , describe "update"
            [ test "Increment" <|
                \() ->
                    update Increment 1 |> equal (2, Cmd.none )
            , fuzz int "IncrementIfOdd" <|
                \model ->
                    let
                        newModel =
                            if model % 2 /= 0 then
                                model + 1
                            else
                                model
                    in
                        update IncrementIfOdd model |> equal ( newModel, Cmd.none )
            , test "IncrementAsync" <|
                \() ->
                    update IncrementAsync 1 |> equal ( 1, incrementAsync )
            , test "Decrement" <|
                \() ->
                    update Decrement 1 |> equal ( 0, Cmd.none )
            ]
        , describe "view"
            [ test "P has the expected text" <|
                \() ->
                    view 1
                        |> fromHtml
                        |> has [ text ("Clicked: 1 times") ]
            ]
        ]
