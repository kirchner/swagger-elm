module Generate.Decoder exposing (..)

import Codegen.Function as Fun exposing (caseof, function, lazy, letin, pipeline)
import Codegen.Literal exposing (string)
import Generate.Utils exposing (decoderName, nestedDecoderName, typeName)
import Json.Decode as Json exposing (decodeString)
import Swagger.Definition as Def exposing (Definition, getFullName, getType)
import Swagger.Type
    exposing
        ( Properties(Properties)
        , Property(Default, Optional, Required)
        , Type(Array_, Bool_, Dict_, Enum_, Float_, Int_, Object_, Ref_, String_)
        , getItemsType
        )


renderDecoder : Definition -> String
renderDecoder definition =
    let
        name =
            getFullName definition
    in
    function (decoderName <| name)
        []
        ("Decoder " ++ typeName name)
        (renderDecoderBody definition)


renderDecoderBody : Definition -> String
renderDecoderBody definition =
    case getType definition of
        Object_ properties ->
            renderObjectBody (getFullName definition) properties

        Array_ items ->
            renderArrayBody (getFullName definition) (getItemsType items)

        Dict_ typeName ->
            renderDictBody (getFullName definition) typeName

        Enum_ _ enum ->
            renderEnumBody (getFullName definition) enum

        String_ _ ->
            renderPrimitiveBody "string"

        Int_ _ ->
            renderPrimitiveBody "int"

        Float_ _ ->
            renderPrimitiveBody "float"

        Bool_ _ ->
            renderPrimitiveBody "bool"

        Ref_ ref ->
            decoderName ref


renderDecoderBodyInline : Definition -> String
renderDecoderBodyInline definition =
    case getType definition of
        Object_ properties ->
            renderObjectBody (getFullName definition) properties

        Array_ items ->
            renderArrayBodyInline (getFullName definition) (getItemsType items)

        Dict_ typeName ->
            renderDictBody (getFullName definition) typeName

        Enum_ _ enum ->
            renderEnumBody (getFullName definition) enum

        String_ _ ->
            renderPrimitiveBody "string"

        Int_ _ ->
            renderPrimitiveBody "int"

        Float_ _ ->
            renderPrimitiveBody "float"

        Bool_ _ ->
            renderPrimitiveBody "bool"

        Ref_ ref ->
            decoderName ref


renderPrimitiveBody : String -> String
renderPrimitiveBody type_ =
    type_


renderArrayBody : String -> Type -> String
renderArrayBody name type_ =
    "list "
        ++ renderPropertyDecoder name "Item" type_
        |> flip pipeline [ "map " ++ typeName name ]


renderArrayBodyInline : String -> Type -> String
renderArrayBodyInline name type_ =
    "list "
        ++ renderPropertyDecoder name "Item" type_


renderDictBody : String -> Type -> String
renderDictBody name type_ =
    "dict " ++ renderPropertyDecoder name "Property" type_


renderObjectBody : String -> Properties -> String
renderObjectBody name (Properties properties) =
    properties
        |> List.map (renderObjectDecoderProperty name)
        |> flip (++) [ "map " ++ typeName name ]
        |> pipeline ((++) "decode " <| typeName (name ++ "Record"))


renderObjectDecoderProperty : String -> Property -> String
renderObjectDecoderProperty parentName property =
    case property of
        Required name type_ ->
            "required " ++ string name ++ " " ++ renderPropertyDecoder parentName name type_

        Optional name type_ ->
            "maybe " ++ string name ++ " " ++ renderPropertyDecoder parentName name type_

        Default name type_ default ->
            "optional "
                ++ string name
                ++ " "
                ++ renderPropertyDecoder parentName name type_
                ++ " "
                ++ defaultValue type_ default


defaultValue : Type -> String -> String
defaultValue type_ default =
    case type_ of
        Enum_ _ _ ->
            case decodeString Json.string default of
                Ok newDefault ->
                    typeName newDefault

                Err err ->
                    Debug.crash "Invalid default value" err default

        _ ->
            default


renderPropertyDecoder : String -> String -> Type -> String
renderPropertyDecoder parentName name type_ =
    case type_ of
        Object_ _ ->
            lazy <| nestedDecoderName parentName name

        Array_ _ ->
            nestedDecoderName parentName name

        Dict_ _ ->
            nestedDecoderName parentName name

        Enum_ _ _ ->
            nestedDecoderName parentName name

        String_ _ ->
            "string"

        Int_ _ ->
            "int"

        Float_ _ ->
            "float"

        Bool_ _ ->
            "bool"

        Ref_ ref ->
            lazy <| decoderName ref


renderEnumBody : String -> List String -> String
renderEnumBody parentName enum =
    let
        decoderName_ =
            decoderName parentName
    in
    letin
        [ ( "decodeToType string"
          , caseof "string"
                (List.map renderEnumEach enum ++ [ renderEnumFail parentName ])
          )
        ]
        "customDecoder string decodeToType"


renderEnumEach : String -> ( String, String )
renderEnumEach value =
    ( string value, "Result.Ok " ++ typeName value )


renderEnumFail : String -> ( String, String )
renderEnumFail parentName =
    ( "_", "Result.Err (\"Invalid value for " ++ typeName parentName ++ ". Value: \" ++ string)" )
