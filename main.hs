module Main where

--import Text.Parsec
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language (emptyDef)
import qualified Text.ParserCombinators.Parsec.Token as TP
import System.Environment (getArgs)
import Data.List

import Less



import Control.Applicative ((<$>), (<*>), (<**>))
--import Control.Monad


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




run p less = case (parse p "" less) of
    Left err -> do
        print err
        error "parse error"
    Right x -> do
        return x


--main = do
--(source, target) <- munchArgs
--readFile source >>= compile >>= writeFile target