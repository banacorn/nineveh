module Less where

import Control.Applicative ((<$>))
import Data.List

import Text.ParserCombinators.Parsec
import Parser
import Value.Value





property = fmap (intercalate "-" . concat) <$> lexeme $ many1 identifier `sepBy` symbol "-"

selector = property

declaration = lexeme $
    do
        p <- property
        symbol ":"
        v <- value
        return (p, v)
    <|> 
    do 
        spaces
        return ("", None)

declarations = declaration `sepBy` symbol ";"

block = lexeme $ do
    s <- selector
    symbol "{"
    ds <- declarations
    symbol "}"
    return (s, ds)

blocks = lexeme $ many block