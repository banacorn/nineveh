module Color (
    Color(..),
    color
    ) where

import Parser
import Text.ParserCombinators.Parsec

import Control.Applicative ((<$>), (<*>), (<**>))

-- datatypes
data Variable = Variable String deriving (Eq, Show)

data Color  = RGB (String, String, String)
            | RGBA (String, String, String, String)
            | HSL (String, String, String)
            | HSLA (String, String, String, String)
            | Hex String
            | Transparent
            | CurrentColor
            | ColorKeyword String
            | ColorVariable Variable
            deriving (Eq, Show)


-- numbers
percentage = lexeme $ do
    n <- many1 digit
    char '%'
    return (n ++ "%")

integerString = show <$> integer

hex = lexeme $ do
    char '#'
    h <- count 3 hexDigit
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

--variable :: Parser Variable
variable = do 
    char '@'
    v <- identifier
    return $ Variable v

color :: Parser Color
color = lexeme $
        hex 
    <|> try rgb 
    <|> try rgba 
    <|> try hsl 
    <|> try hsla    
    <|> ColorKeyword <$> identifier 
    <|> ColorVariable <$> variable