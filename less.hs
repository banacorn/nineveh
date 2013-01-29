module Less where

import Control.Applicative ((<$>))
import Data.List

import Text.ParserCombinators.Parsec
import Parser
import Value.Value
import Value.Variable



--test = do
--    content <- readFile "test/less.less"
--    a <- run rules content
--    putStrLn $ show a

--parseIdentifierWithHyphen = fmap (intercalate "-" . concat) <$> lexeme $ many1 identifier `sepBy` symbol "-"

--property = identifier
--selector = lexeme parseIdentifierWithHyphen

--selectors = selector `sepBy1` comma

--declaration = lexeme $
--    do
--        p <- property
--        symbol ":"
--        v <- parseValue
--        return (p, v)
--    <|> 
--    do 
--        spaces
--        return ("", None)

--declarations = declaration `sepBy` symbol ";"

--block = lexeme $ between (symbol "{") (symbol "}") declarations

--rule = do
--    s <- selectors 
--    b <- block    
--    return (s, b)

--variableDeclaration = lexeme $ do
--    variable <- parseVariable
--    symbol ":" <?> "a colon ':' between variable name and value"
--    value <- parseValue
--    symbol ";" <?> "variable declaration end with semicolon ';'"
--    return (variable, value)

--rules = many rule
