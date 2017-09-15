module Swagger.Swagger exposing (..)

import Swagger.Definition exposing (Definitions)
import Swagger.Path exposing (Paths)


type alias Swagger =
    { definitions : Definitions
    , paths : Paths
    }
