module SvgAst.Encode exposing (encode)

{-|
# Encode an SvgAst

@docs encode
-}

import SvgAst as S
import Json.Encode as JE
import Dict exposing (Dict, toList)

encodeDElement : S.DElement -> JE.Value
encodeDElement de =
    case de of
        S.M x y -> JE.object [
                 ("M", JE.list [JE.float x, JE.float y])
                 ]
        S.L x y -> JE.object [
                 ("L", JE.list [JE.float x, JE.float y])
                 ]

encodeValue : S.Value -> JE.Value
encodeValue val =
    case val of
        S.Value str -> JE.string str
        S.D ds -> JE.list <| List.map encodeDElement ds

encodeAttribute : (S.Key, S.Value) -> JE.Value
encodeAttribute (key, value) =
    JE.object [
         ("key", JE.string key)
       , ("value", encodeValue value)
        ]


{-| Encode an SvgAst
-}
encode : S.SvgAst -> JE.Value
encode ast =
    case ast of
        S.Body _ -> JE.null
        S.Comment _ -> JE.null
        S.Tag name attr children ->
            JE.object [
                 ("name", JE.string name)
               , ("attributes", JE.list <| List.map encodeAttribute (toList attr))
               , ("children", JE.list [])]
