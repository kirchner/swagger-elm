module Generate.Path exposing (renderPaths)

import Generate.Decoder exposing (renderDecoderBodyInline)
import Generate.Type exposing (renderType_)
import List.Extra as List
import String.Extra as String
import Swagger.Definition exposing (definition, getFullName)
import Swagger.Path
    exposing
        ( MediaTypeObject
        , OperationObject(..)
        , OperationObjectData
        , ParameterLocation(..)
        , ParameterObject
        , Path
        , PathElement(..)
        , PathItem
        , Paths
        , ResponseObject
        , ResponseType(..)
        , ResponsesObject
        , delete
        , get
        , head
        , options
        , patch
        , pathElement
        , post
        , put
        , trace
        )


renderPaths : Paths -> String
renderPaths paths =
    paths
        |> List.map renderPath
        |> String.join "\n\n"


renderPath : ( Path, PathItem ) -> String
renderPath ( path, pathItem ) =
    let
        render method prefix forceSingular data =
            let
                notInUrlParameters =
                    data.parameters |> List.filter notInUrl

                notInUrl parameterObject =
                    path
                        |> List.member (PathElementParameter parameterObject.name)
                        |> not

                inUrlParameters =
                    data.parameters |> List.filter inUrl

                inUrl parameterObject =
                    path
                        |> List.member (PathElementParameter parameterObject.name)

                sortedParameters =
                    inUrlParameters ++ notInUrlParameters
            in
            [ renderRequestFunction prefix forceSingular path sortedParameters data.responses
            , [ "let"
              , [ renderPathUrl path
                , renderBody notInUrlParameters
                , renderExpect data.responses
                ]
                    |> String.join "\n\n"
                    |> indent
              , "in"
              , renderRequest method
              ]
                |> String.join "\n"
                |> indent
            ]
                |> String.join "\n"
    in
    [ case pathItem.get of
        Just (OperationObject data) ->
            render "get" "" False data

        Nothing ->
            ""
    , case pathItem.put of
        Just (OperationObject data) ->
            render "put" "update" False data

        Nothing ->
            ""
    , case pathItem.post of
        Just (OperationObject data) ->
            render "post" "create" True data

        Nothing ->
            ""
    , case pathItem.delete of
        Just (OperationObject data) ->
            render "delete" "destroy" False data

        Nothing ->
            ""
    , case pathItem.patch of
        Just (OperationObject data) ->
            render "patch" "update" False data

        Nothing ->
            ""
    ]
        |> String.join "\n\n"


renderRequestFunction :
    String
    -> Bool
    -> Path
    -> List ParameterObject
    -> ResponsesObject
    -> String
renderRequestFunction prefix forceSingular path parameters responsesObject =
    let
        functionName =
            renderRequestFunctionName prefix forceSingular path
    in
    case responsesObject |> List.head of
        Just ( responseType, responseObject ) ->
            let
                returnType =
                    case responseObject.schema of
                        Just tvpe ->
                            definition Nothing "" tvpe
                                |> renderType_

                        Nothing ->
                            "()"
            in
            [ [ functionName
              , " : "
              , [ renderRequestFunctionArguments parameters
                , [ "Request " ++ returnType ]
                ]
                    |> List.concat
                    |> String.join " -> "
              ]
                |> String.concat
            , [ [ functionName
                , parameters
                    |> List.map (.name >> renderParameterObjectName)
                    |> String.join " "
                ]
                    |> String.join " "
              , " ="
              ]
                |> String.concat
            ]
                |> String.join "\n"

        Nothing ->
            Debug.crash "TODO"


renderRequestFunctionName : String -> Bool -> Path -> String
renderRequestFunctionName prefix forceSingular path =
    let
        functionName =
            [ prefix
            , path
                |> List.filterMap
                    (\element ->
                        case element of
                            PathElementText text ->
                                Just (String.classify text)

                            _ ->
                                Nothing
                    )
                |> String.concat
            ]
                |> String.concat
                |> String.decapitalize

        isPlural =
            if (functionName |> String.right 1) == "s" then
                True
            else
                False

        singleResource =
            case path |> List.last of
                Just (PathElementParameter _) ->
                    True

                _ ->
                    False

        singularFunctionName =
            functionName |> String.dropRight 1
    in
    if (singleResource || forceSingular) && isPlural then
        singularFunctionName
    else
        functionName


renderRequestFunctionArguments : List ParameterObject -> List String
renderRequestFunctionArguments parameterObjects =
    parameterObjects
        |> List.map renderRequestFunctionArgument


renderRequestFunctionArgument : ParameterObject -> String
renderRequestFunctionArgument parameterObject =
    [ if not parameterObject.required then
        "Maybe "
      else
        ""
    , "String"
    ]
        |> String.concat


renderPathUrl : Path -> String
renderPathUrl path =
    [ "url ="
    , [ "[ "
      , path
            |> List.map renderPathElement
            |> String.join "\n, "
      , "\n] |> String.concat"
      ]
        |> String.concat
        |> indent
    ]
        |> String.join "\n"


renderPathElement : PathElement -> String
renderPathElement pathElement =
    case pathElement of
        PathElementText text ->
            [ "\"/"
            , text
            , "\""
            ]
                |> String.concat

        PathElementParameter parameter ->
            parameter |> renderParameterObjectName


renderBody : List ParameterObject -> String
renderBody parameterObjects =
    [ "body ="
    , indent <|
        if parameterObjects |> List.isEmpty then
            "Http.emptyBody"
        else
            [ "Http.jsonBody <|"
            , [ "Json.Encode.object"
              , [ "[ "
                , parameterObjects
                    |> List.map renderParameterObject
                    |> String.join "\n, "
                , "\n]"
                ]
                    |> String.concat
                    |> indent
              ]
                |> String.join "\n"
                |> indent
            ]
                |> String.join "\n"
    ]
        |> String.join "\n"


renderParameterObject : ParameterObject -> String
renderParameterObject parameterObject =
    [ "( \""
    , parameterObject.name
    , "\", "
    , "Json.Encode.string <| "
    , if parameterObject.required then
        parameterObject.name |> renderParameterObjectName
      else
        "Maybe.withDefault \"\""
            ++ (parameterObject.name |> renderParameterObjectName)
    , " )"
    ]
        |> String.concat


renderExpect : ResponsesObject -> String
renderExpect responsesObject =
    case responsesObject |> List.head of
        Just ( responseType, responseObject ) ->
            [ "expect ="
            , [ "Http.expectJson"
              , case responseObject.schema of
                    Just tvpe ->
                        [ "("
                        , definition Nothing "identity" tvpe
                            |> renderDecoderBodyInline
                        , ")"
                        ]
                            |> String.concat

                    Nothing ->
                        "(Json.Decode.succeed ())"
              ]
                |> String.join "\n"
                |> indent
            ]
                |> String.join "\n"

        Nothing ->
            Debug.crash "TODO"


renderRequest : String -> String
renderRequest method =
    [ "Http.request"
    , [ "{ method = \"" ++ String.toUpper method ++ "\""
      , ", headers = []"
      , ", url = url"
      , ", body = body"
      , ", expect = expect"
      , ", timeout = Nothing"
      , ", withCredentials = False"
      , "}"
      ]
        |> String.join "\n"
        |> indent
    ]
        |> String.join "\n"


renderParameterObjectName : String -> String
renderParameterObjectName name =
    name
        |> String.camelize
        |> String.decapitalize



---- HELPER


indent : String -> String
indent text =
    text
        |> String.split "\n"
        |> List.map (\line -> "    " ++ line)
        |> String.join "\n"
