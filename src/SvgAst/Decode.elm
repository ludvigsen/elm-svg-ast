module SvgAst.Decode exposing (decode)

import SvgAst as S
import Json.Decode as JD
import Dict exposing (Dict, fromList)

decodeL : JD.Decoder S.DElement
decodeL =
        JD.map2 S.L (JD.index 0 JD.float) (JD.index 1 JD.float)

decodeM : JD.Decoder S.DElement
decodeM =
        JD.map2 S.M (JD.index 0 JD.float) (JD.index 1 JD.float)

decodeDElement : JD.Decoder S.DElement
decodeDElement =
    JD.oneOf [(JD.at ["L"] decodeL), (JD.at ["M"] decodeM)]

decodeD : JD.Decoder S.Value
decodeD =
    JD.map S.D (JD.list decodeDElement)

decodeValue : JD.Decoder S.Value
decodeValue =
    JD.map (S.Value) JD.string

decodeAttribute : JD.Decoder (S.Key, S.Value)
decodeAttribute =
    JD.map2 (,) (JD.field "key" JD.string) (JD.field "value" (JD.oneOf [decodeValue, decodeD]))

decodeAttributes : JD.Decoder (Dict S.Key S.Value)
decodeAttributes =
    JD.map fromList <| JD.list decodeAttribute

decode: JD.Decoder S.SvgAst
decode =
    JD.map3 S.Tag
        (JD.field "name" JD.string)
        (JD.field "attributes" decodeAttributes)
        (JD.field "children" <| JD.list (JD.lazy (\_ -> decode)))
