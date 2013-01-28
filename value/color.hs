-- Refernce from MDN https://developer.mozilla.org/en-US/docs/CSS/color

module Value.Color (
    parseColor
    ) where

import Parser
import Text.ParserCombinators.Parsec

--import Control.Applicative ((<$>), (<*>), (<**>))
import Value.Type
import Value.Instance
import Value.Integer
import Value.Number
import Value.Percentage

parseColor :: Parser Value
parseColor = lexeme $
        try transparent
    <|> try currentColor
    <|> try rgb
    <|> try rgba
    <|> try hsl
    <|> try hsla
    <|> colorKeyword
    <|> hex


transparent = symbol "transparent" >> return Transparent
currentColor = symbol "currentColor" >> return CurrentColor
colorKeyword = do
    i <- identifier 
    return (ColorKeyword i)

hex = 
    try (do
        char '#'
        h <- count 6 hexDigit
        return $ Hex ('#':h))
    <|> (do
        char '#'
        h <- count 3 hexDigit
        return $ Hex ('#':h))

rgb =
    try (do
        string "rgb"
        [r, g, b] <- parens (commaSep1 parsePercentage)
        return $ RGB (r, g, b)
    ) <|> (do
        string "rgb"
        [r, g, b] <- parens (commaSep1 parseInteger)
        return $ RGB (r, g, b)
    )

rgba =
    try (do
        string "rgba"
        parens $ do
            r <- parsePercentage
            comma
            g <- parsePercentage
            comma
            b <- parsePercentage
            comma
            a <- parseNumber
            return (RGBA (r, g, b, a))
    ) <|> (do
        string "rgba"
        parens $ do
            r <- parseInteger
            comma
            g <- parseInteger
            comma
            b <- parseInteger
            comma 
            a <- parseNumber
            return (RGBA (r, g, b, a))
    )

hsl = do
    string "hsl"
    parens $ do
        h <- parseInteger
        comma
        s <- parseInteger
        comma
        l <- parseInteger
        return (HSL (h, s, l))


hsla = do
    string "hsla"
    parens $ do
        h <- parseInteger
        comma
        s <- parseInteger
        comma
        l <- parseInteger
        comma
        a <- parseNumber
        return (HSLA (h, s, l, a))
