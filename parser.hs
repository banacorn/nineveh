module Parser (
    parse,
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
    identifier,
    symbol,

    run
    ) where


import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language (emptyDef)
import qualified Text.ParserCombinators.Parsec.Token as TP


-- lexer
lexer :: TP.TokenParser ()
lexer  = TP.makeTokenParser emptyDef

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

-- In CSS3, identifiers (including element names, classes, and IDs in selectors (see [SELECT] [or is this still true])) 
-- can contain only the characters [A-Za-z0-9] and ISO 10646 characters 161 and higher, 
-- plus the hyphen (-) and the underscore (_); they cannot start with a digit or a hyphen followed by a digit. 
-- They can also contain escaped characters and any ISO 10646 character as a numeric code (see next item). 
-- For instance, the identifier "B&W?" may be written as "B\&W\?" or "B\26 W\3F". (See [UNICODE310] and [ISO10646].)

identifier :: Parser String
identifier =
    try (do 
        x <- firstLetter <?> "start with a letter or a hyphen"
        xs <- many (alphaNum <|> char '-' <|> char '_')
        return (x:xs)
    ) <|> (do
        char '-'
        x <- letter <?> "hyphen followed by a letter"
        xs <- many (alphaNum <|> char '-' <|> char '_')
        return ('-':x:xs)
    )
    where firstLetter = letter

run p less = case (parse p "" less) of
    Left err -> do
        print err
        error "parse error"
    Right x -> do
        return x



