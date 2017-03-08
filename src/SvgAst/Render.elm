module SvgAst.Render exposing (render)

{-|
Module that converts SvgAst to Svg

@docs render
-}

import SvgAst exposing (..)
import Svg as S exposing (Svg)
import Svg.Attributes as A
import Html.Attributes as HA
import Dict exposing (Dict)
import List as L

parseDElement : DElement -> String
parseDElement el =
    case el of
        (M x y) ->
            "M " ++ (toString x) ++ " " ++ (toString y)
        (L x y) ->
            "L " ++ (toString x) ++ " " ++ (toString y)

parseAttribute : (Key, Value) -> S.Attribute msg
parseAttribute attr =
    case attr of
        (key, D elms) ->
            A.d (String.join " " (L.map parseDElement elms))
        (string, Value val) ->
            HA.attribute string val

parseAttributes : (Dict Key Value) -> List (S.Attribute msg)
parseAttributes attrs =
    L.map parseAttribute (Dict.toList attrs)

{-| Render an SvgAst
-}
render : SvgAst -> Svg msg
render ast =
    case ast of
        Tag name attrs children ->
            S.node name (parseAttributes attrs) (L.map render children)
        Body string ->
            S.text string
        Comment string ->
            S.g [] []
