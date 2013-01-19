-- Refernce from MDN https://developer.mozilla.org/en-US/docs/CSS/shape

module Value.Shape (
    Shape(..),
    shape
    ) where

import Parser
import Text.ParserCombinators.Parsec

import Value.Length

data Shape = Shape (Length, Length, Length, Length)

instance Show Shape where
    show (Shape (top, right, bottom, left)) = 
        "rect(" ++ show top ++ ", " ++ show right ++ ", " ++ show bottom ++ ", " ++ show left ++ ")"

shape = lexeme $ do
    string "rect"
    [top, right, bottom, left] <- parens (length' `sepBy` symbol ",")
    return (Shape (top, right, bottom, left))