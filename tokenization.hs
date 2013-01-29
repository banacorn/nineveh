-- Reference: Tokenization http://www.w3.org/TR/css3-syntax/#tokenization

module Tokenization where

import Control.Applicative ((<$>))
import Data.Char (ord)
import Data.List (intercalate)
import Text.ParserCombinators.Parsec


run p less = case (parse p "" less) of
    Left err -> do
        print err
        error "parse error"
    Right x -> do
        return x

nmstart :: Parser String
nmstart = return <$> letter <|> string "_" <|> return <$> nonascii <|> escape

nmchar :: Parser String
nmchar = nmstart <|> string "-"

num :: Parser String
num = try (do
        real <- many digit
        char '.'
        frac <- many1 digit
        return (real ++ "." ++ frac)
    ) <|> many1 digit

nl :: Parser String
nl  =   try (do
        char '\xA'
        char '\xD'
        return "\xA\xD"
    )
    <|> return <$> char '\xA'
    <|> return <$> char '\xC' 
    <|> return <$> char '\xD'

wc :: Parser Char
wc = oneOf "\x9\xA\xC\xD\x20"



w :: Parser String
w = many wc

nonascii :: Parser Char
nonascii = satisfy (\ c -> let n = ord c in
            n >= 0x80 && n <= 0xD7FF
        ||  n >= 0xE000 && n <= 0xFFFD
        ||  n >= 0x10000 && n <= 0x10FFFF
    )

unicode :: Parser String
unicode = do
    char '\\'
    body <- many1 alphaNum
    ending <- option "" (return <$> wc)
    return ('\\':(body ++ ending))

escape :: Parser String
escape = unicode <|> do
    char '\\'
    i <- satisfy (\ c -> let n = ord c in
                n >= 0x20 && n <= 0x7E
            ||  n >= 0x80 && n <= 0xD7FF
            ||  n >= 0xE000 && n <= 0xFFFD
            ||  n >= 0x10000 && n <= 0x10FFFF

            &&  n /= 0x27 && n /= 0x22  -- ' and "
        )
    return ('\\' : [i])

urlchar :: Parser String
urlchar = return <$> nonascii <|> escape <|> return <$> satisfy (\ c -> let n = ord c in
                n >= 0x09 && n <= 0x21
            ||  n >= 0x23 && n <= 0x7E
            
            &&  n /= 0x27 && n /= 0x22   -- ' and "
        )

stringchar :: Parser String
stringchar = urlchar <|> string "\x20" <|> (do
    char '\\'
    n <- nl
    return ('\\':n))
--stringchar = urlchar <|> string "\x20" <|> (do
--    char '\\'
--    n <- nl
--    return ('\\':n))



-- TOKENS


identifier :: Parser String
identifier = do
        hyphen <- option "" (string "-")
        start <- nmstart <?> "starts with a letter or '_' or a nonascii or an escape"
        rest <- many nmchar
        return (hyphen ++ start ++ concat rest)

atKeyword :: Parser String
atKeyword = do
    char '@'
    i <- identifier
    return ('@':i)

string' :: Parser String
string' = do
            char '"'
            str <- concat <$> many (stringchar <|> string "'")
            char '"'
            return ("\"" ++ str ++ "\"")
        <|> do 
            char '\''
            str <- concat <$> many (stringchar <|> string "\"")
            char '\''
            return ("'" ++ str ++ "'")
