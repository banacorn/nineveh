module Main where

--import Text.Parsec
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language (emptyDef)
import qualified Text.ParserCombinators.Parsec.Token as T
import System.Environment (getArgs)
import Data.List

--lexer :: T.TokenParser ()
--lexer  = T.makeTokenParser emptyDef

--whiteSpace  = T.whiteSpace lexer
--lexeme      = T.lexeme lexer
--symbol      = T.symbol lexer
--natural     = T.natural lexer
--parens      = T.parens lexer
--semi        = T.semi lexer

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

main = do
    arguments <- munchArgs
    putStrLn . show $ arguments
