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
semi            = TP.semi lexer
identifier      = TP.identifier lexer
natural         = TP.natural lexer
symbol          = TP.symbol lexer






run p less = case (parse p "" less) of
    Left err -> do
        print err
        error "parse error"
    Right x -> do
        return x



