module Swagger.Path
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

import Dict exposing (Dict)
import Regex exposing (HowMany(AtMost))
import Swagger.Type exposing (Type)


type alias Paths =
    List ( Path, PathItem )


type alias Path =
    List PathElement


type PathElement
    = PathElementText String
    | PathElementParameter String


type alias PathItem =
    { summary : Maybe String
    , description : Maybe String
    , get : Maybe (OperationObject Get)
    , put : Maybe (OperationObject Put)
    , post : Maybe (OperationObject Post)
    , delete : Maybe (OperationObject Delete)
    , options : Maybe (OperationObject Options)
    , head : Maybe (OperationObject Head)
    , patch : Maybe (OperationObject Patch)
    , trace : Maybe (OperationObject Trace)
    , parameters : List ParameterObject
    }


type OperationObject tvpe
    = OperationObject OperationObjectData


type alias OperationObjectData =
    { summary : Maybe String
    , description : Maybe String
    , parameters : List ParameterObject
    , responses : ResponsesObject
    }


type Get
    = Get


type Put
    = Put


type Post
    = Post


type Delete
    = Delete


type Options
    = Options


type Head
    = Head


type Patch
    = Patch


type Trace
    = Trace


type alias ParameterObject =
    { name : String
    , in_ : ParameterLocation
    , description : Maybe String
    , required : Bool
    }


type ParameterLocation
    = ParameterLocationQuery
    | ParameterLocationHeader
    | ParameterLocationPath
    | ParameterLocationCookie


type alias ResponsesObject =
    List ( ResponseType, ResponseObject )


type ResponseType
    = DefaultResponse
    | HttpStatus Int


type alias ResponseObject =
    { description : String
    , content : Dict String MediaTypeObject
    , schema : Maybe Type
    }


type alias MediaTypeObject =
    { schema : Type }


pathElement : String -> PathElement
pathElement element =
    case
        element
            |> Regex.find (AtMost 1) (Regex.regex "{(\\w+)}")
            |> List.head
    of
        Nothing ->
            PathElementText element

        Just parameter ->
            parameter.submatches
                |> List.head
                |> Maybe.map (Maybe.withDefault parameter.match)
                |> Maybe.withDefault parameter.match
                |> PathElementParameter


get : OperationObjectData -> OperationObject Get
get data =
    OperationObject data


put : OperationObjectData -> OperationObject Put
put data =
    OperationObject data


post : OperationObjectData -> OperationObject Post
post data =
    OperationObject data


delete : OperationObjectData -> OperationObject Delete
delete data =
    OperationObject data


options : OperationObjectData -> OperationObject Options
options data =
    OperationObject data


head : OperationObjectData -> OperationObject Head
head data =
    OperationObject data


patch : OperationObjectData -> OperationObject Patch
patch data =
    OperationObject data


trace : OperationObjectData -> OperationObject Trace
trace data =
    OperationObject data
