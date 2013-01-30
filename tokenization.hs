-- Reference: Tokenization http://www.w3.org/TR/css3-syntax/#tokenization

module Tokenization (
    identifier,
    string',
    function

    ) where

import Control.Applicative ((<$>))
import Data.Char (ord)
import Data.List (intercalate)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language (emptyDef)
import qualified Text.ParserCombinators.Parsec.Token as TP

-- lexer
lexer :: TP.TokenParser ()
lexer  = TP.makeTokenParser emptyDef

parens          = TP.parens lexer


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

name :: Parser String
name = concat <$> many1 nmchar

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
    ending <- option "" (return <$> space)
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

--
--      TOKENS
--

--data Identifier = Identifier String
--data AtKeyword = AtKeyword String
--data StringLiteral = StringLiteral String
--data Hash = Hash String
--data Dimension = Dimension Int String

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

hash :: Parser String
hash = do
    char '#'
    n <- name
    return ('#':n)

number :: Parser String
number = num

percentage :: Parser String
percentage = do
    n <- num
    char '%'
    return (n ++ "%")

--dimension :: Parser (String, String)
dimension = do
    n <- num
    i <- identifier
    return (n, identifier)

function p = do
    func <- identifier
    s <- parens p
    return (func, s)

