module Main where

--import Text.Parsec
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language (emptyDef)
import qualified Text.ParserCombinators.Parsec.Token as TP
import System.Environment (getArgs)
import Data.List
import Control.Applicative ((<$>), (<*>))

lexer :: TP.TokenParser ()
lexer  = TP.makeTokenParser emptyDef

lexeme          = TP.lexeme lexer
integer         = TP.integer lexer
float           = TP.float lexer
naturalOrFloat  = TP.naturalOrFloat lexer
stringLiteral   = TP.stringLiteral lexer
parens          = TP.parens lexer
commaSep1       = TP.commaSep1 lexer
comma           = TP.comma lexer

type Path = String
type Arguments = (String, String)

munchArgs :: IO Arguments
munchArgs = do
    args <- getArgs
    case args of
        [source, target] -> 
            return (source, target)
        otherwise ->
            error "less2stylus [-bsc] <source> <target>"

--type Property = String
data Color  = RGB (String, String, String)
            | RGBA (String, String, String, String)
            | HSL (String, String, String)
            | HSLA (String, String, String, String)
            | Hex String
            | ColorKeyword String
            | ColorVariable Variable
            deriving (Eq, Show)

--type LESSUnit = Int
--type LESSSelector = String
--data LESSValue = LESSColor | LESSUnit
--type LESSValue = LESSColor
--type LESSDeclaration = (LESSProperty, LESSValue)
--type LESSBlock = (LESSSelector, [LESSDeclaration])
data Variable = Variable String deriving (Eq, Show)

--alphaNumHyphen :: Parser Char
--alphaNumHyphen = alphaNum <|> char '-'

--property :: Parser LESSProperty
--property = many1 alphaNumHyphen

--percentage

--integer :: Parser Int
--integer = read <$> many1 digit

--rgb :: Parser LESSColor

percentage :: Parser String
percentage = lexeme $ do
    n <- many1 digit
    char '%'
    return (n ++ "%")
    
word = many1 letter
integerString = show <$> integer

hex = lexeme $ do
    char '#'
    h <- integerString
    return $ Hex ('#':h)

rgb = lexeme $ do
    try $ do
        string "rgb"
        [r, g, b] <- parens (commaSep1 percentage)
        return $ RGB (r, g, b)

    <|> do
        string "rgb"
        [r, g, b] <- parens (commaSep1 integerString)
        return $ RGB (r, g, b)

hsl = lexeme $ do
    string "hsl"
    parens $ do
        h <- integerString
        comma
        s <- percentage
        comma
        l <- percentage
        return $ HSL (h, s, l)

rgba = lexeme $ do
    try $ do
        string "rgba"
        parens $ do
            r <- percentage
            comma
            g <- percentage
            comma
            b <- percentage
            comma 
            a <- naturalOrFloat
            case a of
                Left i -> return $ RGBA (r, g, b, show i)
                Right f -> return $ RGBA (r, g, b, show f)
    <|> do
        string "rgba"
        parens $ do
            r <- integerString
            comma
            g <- integerString
            comma
            b <- integerString
            comma 
            a <- naturalOrFloat
            case a of
                Left i -> return $ RGBA (r, g, b, show i)
                Right f -> return $ RGBA (r, g, b, show f)

hsla = lexeme $ do
    string "hsl"
    parens $ do
        h <- integerString
        comma
        s <- percentage
        comma
        l <- percentage
        comma
        a <- naturalOrFloat
        case a of
            Left i -> return $ HSLA (h, s, l, show i)
            Right f -> return $ HSLA (h, s, l, show f)

colorKeyword = lexeme $ many1 alphaNum

variable :: Parser Variable
variable = lexeme $ do
    char '@'
    v <- word
    return (Variable v)

color :: Parser Color
color = lexeme $
        hex 
    <|> try rgb 
    <|> try rgba 
    <|> try hsl 
    <|> try hsla    
    <|> ColorKeyword <$> colorKeyword 
    <|> ColorVariable <$> variable


run p less = case (parse p "" less) of
    Left err -> do
        print err
        error "parse error"
    Right x -> do
        return x


--main = do
--(source, target) <- munchArgs
--readFile source >>= compile >>= writeFile target