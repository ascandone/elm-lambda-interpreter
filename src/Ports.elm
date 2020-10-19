port module Ports exposing (..)

import Json.Decode exposing (Value)


port onWindowScroll : (Value -> msg) -> Sub msg
