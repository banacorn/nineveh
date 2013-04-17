-- Reference: http://www.w3.org/TR/css3-syntax/#grammar

module Grammar where

import Data.List
import Data.Char (toLower)
import Text.ParserCombinators.Parsec

import Tokenize
import Value
import Selector



data Declaration = Declaration String Value
                 | NothingDeclared
                 deriving (Eq)

data Ruleset = Ruleset [Selector] [Declaration] deriving (Eq)

data Stylesheet = Stylesheet [Ruleset] deriving (Eq)

instance Show Declaration where
    show (Declaration property value) = property ++ ": " ++ show value
    show (NothingDeclared) = ""

instance Show Ruleset where
    show (Ruleset selectors []) = intercalate ", " (fmap show selectors) ++ " {}"
    show (Ruleset selectors [NothingDeclared]) = intercalate ", " (fmap show selectors) ++ " {}"
    show (Ruleset selectors declarations) = 
        intercalate ", " (fmap show selectors) ++
        " {" ++
        showDeclarations declarations ++
        "\n}"
        where   showDeclarations [] = ""
                showDeclarations (NothingDeclared:xs) = showDeclarations xs
                showDeclarations (x:xs) = "\n   " ++ show x ++ ";" ++ showDeclarations xs

instance Show Stylesheet where
    show (Stylesheet rulesets) = intercalate "\n\n" (fmap show rulesets)

test = do
    c <- readFile "test/less.less"
    run parseStylesheet c


parseProperty = lexeme identifier



parseDeclaration = (do
        property <- parseProperty
        symbol ":"
        value <- lexeme parseValue
        return (Declaration property value)
    ) <|> (spaces >> return NothingDeclared)

parseRuleset = do
    selectors <- parseSelector `sepBy1` symbol ","
    symbol "{"
    declarations <- parseDeclaration `sepBy` symbol ";"
    symbol "}"
    return (Ruleset selectors declarations)

parseStylesheet = many (lexeme parseRuleset) >>= return . Stylesheet

--parseBlock = between (symbol "{") (symbol "}") (many (noneOf "}"))
