module Generate.Swagger exposing (..)

import Codegen.Utils exposing (capitalize, sanitize)
import Generate.Decoder exposing (renderDecoder)
import Generate.Encoder exposing (renderEncoder)
import Generate.Headers exposing (renderHeaders)
import Generate.Path exposing (renderPaths)
import Generate.Type exposing (renderType)
import Swagger.Definition as Def exposing (Definition, getName)
import Swagger.Swagger exposing (Swagger)


render : Swagger -> String
render { definitions, paths } =
    [ renderHeaders
    , paths
        |> renderPaths
    , Def.map renderDefinition definitions
        |> String.concat
    ]
        |> String.join "\n\n"


renderDefinition : Definition -> String
renderDefinition definition =
    let
        name_ =
            moduleName <| getName definition
    in
    String.concat
        [ renderType definition
        , renderDecoder definition
        , renderEncoder definition
        , "\n\n"
        ]


moduleName : String -> String
moduleName =
    capitalize << sanitize
