-- Refernce from MDN https://developer.mozilla.org/en-US/docs/CSS/angle

module Value.Angle (
    Angle(..),
    angle
    ) where

import Parser
import Text.ParserCombinators.Parsec

import Value.Number

data Angle  = Deg Number
            | Grad Number
            | Rad Number
            | Turn Number

instance Show Angle where
    show (Deg n) = show n ++ "deg"
    show (Grad n) = show n ++ "grad"
    show (Rad n) = show n ++ "rad"
    show (Turn n) = show n ++ "turn"


unit =  (string "deg" >> return Deg)
    <|> (string "grad" >> return Grad)
    <|> (string "rad" >> return Rad)
    <|> (string "turn" >> return Turn)

angle = lexeme $ do
    n <- number
    ctor <- unit
    return (ctor n)