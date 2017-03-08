module SvgAst.Parser exposing (parse, parser)

{-|
Module that parses svg from a string

@docs parse, parser
-}

import Combine exposing (..)
import Combine.Char exposing (..)
import Combine.Num as CN
import String
import Result
import SvgAst exposing (..)
import Dict


cr : Parser s Char
cr = satisfy ((==) '\r') <?> "expected newline"

spaces : Parser s (List Char)
spaces =
  many (space <|> newline <|> tab <|> cr)

letter : Parser s Char
letter =
  upper <|> lower

betweenBoth : Char -> Parser s String
betweenBoth ch =
  String.fromList
    <$> between
          (char ch)
          (char ch)
          (many1 ((noneOf [ ch ])) <|> succeed [])

betweenSingleQuotes : Parser s String
betweenSingleQuotes =
  betweenBoth '\''


betweenDoubleQuotes : Parser s String
betweenDoubleQuotes =
  betweenBoth '"'


quotedString : Parser s String
quotedString =
  betweenSingleQuotes <|> betweenDoubleQuotes


attributeName : Parser s String
attributeName =
  String.fromList
    <$> many1 (letter <|> digit <|> char '-' <|> char ':')
    <?> "Invalid Attribute name"


tagName : Parser s String
tagName =
  String.fromList
    <$> many (choice [ letter, digit, char '_', char ':' ])
    <?> "Invalid Tag name"

melement : Parser s DElement
melement =
    (\c x y -> M x y)
      <$> (spaces *> (choice [satisfy ((==) 'M'), satisfy ((==) 'm')]) <* spaces)
      <*> (CN.float <* spaces)
      <*> (CN.float <* spaces)

lelement : Parser s DElement
lelement =
    (\c x y -> L x y)
      <$> (spaces *> (choice [satisfy ((==) 'L'), satisfy ((==) 'l')]) <* spaces)
      <*> (CN.float <* spaces)
      <*> (CN.float <* spaces)

delement : Parser s Value
delement =
    between
      (choice [char '\'', char '"'])
      (choice [char '\'', char '"'])
      ((\els -> (D els))
        <$> (spaces *> (many (choice [melement, lelement])) <* spaces))

quotedStringValue : Parser s Value
quotedStringValue =
    (\s -> Value s)
        <$> quotedString

value : Parser s Value
value =
    delement <|> quotedStringValue

keyValue : Parser s ( String, Value )
keyValue =
  (\key value -> ( key, value ))
    <$> (attributeName <* spaces <* char '=' <* spaces)
    <*> (value <* spaces)


openTag : Parser s ( String, List ( String, Value ) )
openTag =
  (\name attribs -> ( name, attribs ))
    <$> (char '<' *> tagName)
    <*> (spaces *> many keyValue <* char '>')


closeTag : String -> Parser s ()
closeTag str =
  ()
    <$ (string "</" *> spaces *> string str *> spaces *> char '>')
    <?> ("Expected closing Tag for " ++ toString str)


comment : Parser s SvgAst
comment =
  (String.fromList >> String.trim >> Comment)
    <$> (string "<!--" *> manyTill anyChar (string "-->"))

innerSvg : Parser s SvgAst
innerSvg =
  comment <|> svgParser <|> parseBody

withExplicitCloseTag : Parser s SvgAst
withExplicitCloseTag =
  (\( name, attribs, svg ) -> Tag name (Dict.fromList attribs) svg)
    <$> ((openTag <* spaces) >>= \( name, attribs ) -> (\svg -> ( name, attribs, svg )) <$> (many (innerSvg <* spaces) <* closeTag name))

withoutExplicitCloseTag : Parser s SvgAst
withoutExplicitCloseTag =
  (\name attribs -> Tag name (Dict.fromList attribs) [])
    <$> ((char '<' *> tagName <* spaces))
    <*> (many keyValue <* string "/>")

parseBody : Parser s SvgAst
parseBody =
  (Body << String.trim << String.fromList) <$> (many1 (noneOf [ '<', '>' ]))

xmlDeclaration : Parser s ()
xmlDeclaration =
  () <$ (string "<?xml" *> Combine.while ((/=) '?') <* string "?>")

svgDeclaration : Parser s ()
svgDeclaration =
  () <$ (string "<!DOCTYPE" *> Combine.while ((/=) '>') <* string ">")

svgParser : Parser s SvgAst
svgParser =
    (lazy (\()-> withExplicitCloseTag)) <|> (lazy (\()-> withoutExplicitCloseTag))

rootElements : Parser s (List SvgAst)
rootElements =
  many1 (choice [ svgParser, comment <* spaces ])

{-| SvgAst Parser
-}
parser : Parser s (List SvgAst)
parser =
  spaces *> maybe svgDeclaration *> maybe xmlDeclaration *> spaces *> rootElements <* spaces <* end


formatError : List String -> InputStream -> String
formatError ms stream =
  let
    location = currentLocation stream
    separator = "|> "
    expectationSeparator = "\n  * "
    lineNumberOffset = floor (logBase 10 (toFloat location.line)) + 1
    separatorOffset = String.length separator
    padding = location.column + separatorOffset + 2
  in
  "Parse error around line:\n\n"
    ++ toString location.line ++ separator ++ location.source ++ "\n"
    ++ String.padLeft padding ' ' "^"
    ++ "\nI expected one of the following:\n"
    ++ expectationSeparator
    ++ String.join expectationSeparator ms

{-| Parse an SvgAst
-}
parse : String -> Result.Result String (List SvgAst)
parse str =
  case Combine.parse parser str of
    Ok (_, _, svg) ->
      Result.Ok svg
    Err (_, stream, ms) ->
      Result.Err <| formatError ms stream

