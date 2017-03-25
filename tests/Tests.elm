module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (int)
import Counter exposing (..)
import Time exposing (Time, second)
import Test.Html.Query as Query
import Test.Html.Selector exposing (text, tag)


all : Test
all =
    describe "Counter Test Suite"
        [ test "init" <|
            \() ->
                Expect.equal init ( Model 0 False, Cmd.none )
        , describe "update"
            [ test "Increment" <|
                \() ->
                    Expect.equal (update Increment (Model 0 False)) ( Model 1 False, Cmd.none )
            , fuzz int "IncrementIfOdd" <|
                \i ->
                    let
                        expect =
                            if i % 2 /= 0 then
                                i + 1
                            else
                                i
                    in
                        update IncrementIfOdd (Model i False) |> Expect.equal ( Model expect False, Cmd.none )
            , test "IncrementAsync" <|
                \() ->
                    Expect.equal (update IncrementAsync (Model 0 False)) ( Model 0 True, Cmd.none )
            , test "IncrementAsyncComplete" <|
                \() ->
                    Expect.equal (update (IncrementAsyncComplete 1.0) (Model 0 True)) ( Model 1 False, Cmd.none )
            , test "Decrement" <|
                \() ->
                    Expect.equal (update Decrement (Model 1 False)) ( Model 0 False, Cmd.none )
            ]
        , describe "subscriptions"
            [ test "waitingIncrementAsync" <|
                \() ->
                    Expect.equal (subscriptions (Model 0 True)) (Time.every second IncrementAsyncComplete)
            , test "not waitingIncrementAsync" <|
                \() ->
                    Expect.equal (subscriptions (Model 0 False)) (Sub.none)
            ]
        , describe "view"
            [ test "P has the expected text" <|
                \() ->
                    view (Model 1 False)
                        |> Query.fromHtml
                        |> Query.has [ text "Clicked: 1 times" ]
            ]
        ]
