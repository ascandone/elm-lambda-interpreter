module Parsing exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Lambda.Ast exposing (Lambda(..))
import Lambda.Parser exposing (ParseError(..), parse)
import Test exposing (..)


suite : Test
suite =
    concat
        [ describe "Parse primitives"
            [ test "variable" <|
                \_ ->
                    parse "x"
                        |> Expect.equal (Ok <| Variable "x")
            , test "abstraction" <|
                \_ ->
                    parse "\\x.x"
                        |> Expect.equal (Ok <| Abstraction "x" <| Variable "x")
            , test "application" <|
                \_ ->
                    parse "x x"
                        |> Expect.equal (Ok <| Application (Variable "x") (Variable "x"))
            ]
        , describe "Syntactic sugare"
            [ test "application of multiple terms" <|
                \_ ->
                    parse "x y z"
                        |> Expect.equal
                            (Ok <|
                                Application
                                    (Application (Variable "x") (Variable "y"))
                                    (Variable "z")
                            )
            , test "abstraction with multiple bindings" <|
                \_ ->
                    parse "\\x y z.x"
                        |> Expect.equal
                            (Ok <|
                                Abstraction "x" <|
                                    Abstraction "y" <|
                                        Abstraction "z" <|
                                            Variable "x"
                            )
            , describe "Aliases"
                [ test "simple" <|
                    \_ ->
                        parse "let A = a\nA"
                            |> Expect.equal (Ok <| Variable "a")
                , test "mutual recursion" <|
                    \_ ->
                        parse "let A = B\nlet B = b\nx"
                            |> Expect.err
                ]
            ]
        ]
