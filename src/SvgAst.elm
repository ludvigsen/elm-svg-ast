module SvgAst exposing (Attributes, Key, SvgAst(..), Value(..), DElement(..), map, changeAttribute, fold, getStringAttribute, updateAttribute, getAttributeString)

{-|
# SvgAst

Module for parsing and building up an Ast of an SVG. Can also serialize and deserialize to JSON.

@docs Attributes, DElement, Key, SvgAst, Value, map, changeAttribute, fold, getStringAttribute, updateAttribute, getAttributeString
-}

import Dict exposing (Dict)
import List as L
import Maybe as M


{-| Key of an attribute
-}
type alias Key =
    String


{-| D attribute in path elements
-}
type DElement
    = M Float Float
    | L Float Float


{-| Value of an attribute
-}
type Value
    = D (List DElement)
    | Value String


{-| Svg Attribute
-}
type Attributes
    = Dict Key Value


{-| Svg TagName
-}
type alias TagName =
    String


{-| SvgAst type
-}
type SvgAst
    = Tag TagName (Dict Key Value) (List SvgAst)
    | Body String
    | Comment String


{-| map an SvgAst
-}
map : (SvgAst -> SvgAst) -> SvgAst -> SvgAst
map fn ast =
    case ast of
        Tag name attrs children ->
            Tag name attrs (L.map (map fn) children) |> fn

        _ ->
            fn ast


{-| fold an SvgAst
-}
fold : (SvgAst -> a -> a) -> a -> SvgAst -> a
fold fn base ast =
    case ast of
        Tag name attrs children ->
            fn (Tag name attrs children) (L.foldl (\ast n -> fold fn n ast) base children)

        _ ->
            base


{-| Change an attribute of an SvgAst
-}
changeAttribute : ( Key, Value ) -> SvgAst -> SvgAst
changeAttribute ( key, value ) ast =
    case ast of
        Tag name attrs children ->
            Tag name
                (Dict.update key
                    (\x ->
                        if x == Nothing then
                            Nothing
                        else
                            Just value
                    )
                    attrs
                )
                children

        _ ->
            ast


{-| Update an attribute of an SvgAst
-}
updateAttribute : Key -> (Value -> Value) -> SvgAst -> SvgAst
updateAttribute key f ast =
    case ast of
        Tag name attrs children ->
            Tag name
                (Dict.update key
                    (\x ->
                        if x == Nothing then
                            Nothing
                        else
                            M.map f x
                    )
                    attrs
                )
                children

        _ ->
            ast


{-| Get a string attribute as a string
-}
getAttributeString : String -> SvgAst -> Maybe String
getAttributeString key ast =
    case ast of
        Tag name attrs children ->
            getStringAttribute key attrs

        _ ->
            Nothing


{-| Get a string attribute as a string
-}
getStringAttribute : String -> Dict Key Value -> Maybe String
getStringAttribute key attrs =
    let
        value =
            Dict.get key attrs
    in
        case value of
            Just (Value val) ->
                Just val

            _ ->
                Nothing
