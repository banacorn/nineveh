module Less where

import Control.Applicative ((<$>))
import Data.List

import Text.ParserCombinators.Parsec
import Parser
import Value.Color
import Value.Number
import Value.Integer
import Value.Percentage
import Value.Length
import Value.Position
import Value.Ratio
import Value.Frequency
import Value.Resolution
import Value.Shape
import Value.Angle
import Value.String







run p less = case (parse p "" less) of
    Left err -> do
        print err
        error "parse error"
    Right x -> do
        return x


property = fmap (intercalate "-" . concat) <$> lexeme $ many1 identifier `sepBy` symbol "-"

selector = property
value = color

declaration = lexeme $
    do
        p <- property
        symbol ":"
        v <- value
        return (p, v)
    <|> 
    do 
        spaces
        return ("", ColorKeyword "")

declarations = declaration `sepBy` symbol ";"

block = lexeme $ do
    s <- selector
    symbol "{"
    ds <- declarations
    symbol "}"
    return (s, ds)