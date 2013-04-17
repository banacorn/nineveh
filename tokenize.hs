-- Reference: Tokenization http://www.w3.org/TR/css3-syntax/#tokenization

module Tokenize (
    identifier,
    atKeyword,
    string',
    hash,
    number,
    percentage,
    function,
    url,
    unicodeRange,

    lexeme,
    natural,
    integer,
    float,
    naturalOrFloat,
    stringLiteral,
    parens,
    commaSep1,
    comma,
    semi,
    symbol,
    reserved,
    
    run

    ) where

import Control.Applicative ((<$>))
import Data.Char (ord)
import Data.List (intercalate, (!!))
import Text.ParserCombinators.Parsec
--import Text.ParserCombinators.Parsec.Language (emptyDef)
import qualified Text.ParserCombinators.Parsec.Token as TP

-- lexer
languageDef = TP.LanguageDef {
    TP.commentStart    = "/*",
    TP.commentEnd      = "*/",
    TP.commentLine     = "//",
    TP.nestedComments  = True,
    -- fuck it we're not using it anyway
    TP.identStart      = anyChar,
    TP.identLetter     = anyChar,
    TP.opStart         = anyChar,
    TP.opLetter        = anyChar,
    TP.reservedNames   = ["rgb"],
    TP.reservedOpNames = [],
    --TP.identStart      = (char '-' <|> fmap (!! 0) nmstart),
    TP.caseSensitive   = False
}

lexer :: TP.TokenParser ()
lexer  = TP.makeTokenParser languageDef

-- binded with the lexer
lexeme          = TP.lexeme lexer
integer         = TP.integer lexer
float           = TP.float lexer
naturalOrFloat  = TP.naturalOrFloat lexer
stringLiteral   = TP.stringLiteral lexer
parens          = TP.parens lexer
commaSep1       = TP.commaSep1 lexer
comma           = TP.comma lexer
--alphaNum        = TP.alphaNum lexer
semi            = TP.semi lexer
--identifier      = TP.identifier lexer
natural         = TP.natural lexer
symbol          = TP.symbol lexer
reserved        = TP.reserved lexer

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

            &&  n /= 0x27 && n /= 0x22 && n /= 0x29   -- ' and " and )
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
atKeyword = char '@' >> identifier

string' :: Parser String
string' = do
            char '"'
            str <- concat <$> many (stringchar <|> string "'")
            char '"'
            return str
        <|> do 
            char '\''
            str <- concat <$> many (stringchar <|> string "\"")
            char '\''
            return str

hash :: Parser String
hash = char '#' >> identifier

number :: Parser String
number = num

percentage :: Parser String
percentage = do
    n <- num
    char '%'
    return n

dimension = do
    n <- num
    i <- identifier
    return (n, identifier)

function p = do
    func <- identifier
    s <- parens p
    return (func, s)

url = do
    string "url"
    parens (try string' <|> concat <$> many urlchar)

unicodeRange :: Parser String
unicodeRange = do
    string "U+"
    a <- option "" (many1 hexDigit)
    b <- option "" (char '-' >> many1 hexDigit)
    return ("U+" ++ a ++ b)

cdo :: Parser String
cdo = string "<!--"

cdc :: Parser String
cdc = string "-->"

bom :: Parser Char
bom = char '\xFEFF'