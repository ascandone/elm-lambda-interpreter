module Evaluation exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Lambda.Ast exposing (Lambda(..))
import Lambda.Parser exposing (ParseError(..), parse)
import Lambda.Semantics exposing (reduction)
import Test exposing (..)


suite : Test
suite =
    concat
        [ describe "Base case"
            []
        ]
