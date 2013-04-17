module Scanner where

import Text.ParserCombinators.Parsec

--
--  Data Type
--

data Token  = Comment String
              deriving (Eq, Show)

--
--
--


run p less = case (parse p "" less) of
    Left err -> do
        print err
        error "parse error"
    Right x -> do
        return x

lexeme p = do
    s <- p
    spaces
    return s

symbol p = lexeme (string p)

--tokenize :: Parser  
--tokenize p = 

comment :: Parser Token
comment = do
    s <- between (string "/*") (string "*/") (many anyChar)
    return (Comment s)
