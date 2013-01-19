module Parser (
    parse,
    lexeme,
    integer,
    float,
    naturalOrFloat,
    stringLiteral,
    parens,
    commaSep1,
    comma,
    semi,
    identifier,
    symbol
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
symbol          = TP.symbol lexer





