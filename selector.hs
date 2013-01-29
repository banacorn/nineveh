-- Reference: Selectors Level 3 http://www.w3.org/TR/css3-selectors/

module Selector where

import Control.Applicative ((<$>))
import Data.List (intercalate)
import Text.ParserCombinators.Parsec
import Parser


data Selector   = SelectorSequence
                | SelectorSequence `Descendent` Selector
                | SelectorSequence `Child` Selector
                | SelectorSequence `Sibling` Selector 
                deriving (Eq, Show)

data SelectorSequence   = TypeSelector String [OtherSelector]
                        | UniversalSelector [OtherSelector]
                        deriving (Eq, Show)

data OtherSelector  = IDSelector String
                    | ClassSelector String 
                    | AttributeSelector String AttributeOperator String
                    | PsuedoSelector String
                    | NegationSelector String
                    deriving (Eq, Show)

data AttributeOperator = Equal | Includes | PrefixMatch | SuffixMatch | SubstringMatch | DashMatch deriving (Eq, Show)

--selector = simpleSelector `sepBy1` combinator


typeSelector = identifier
universalSelector = symbol "*"

--selectors = selector `sepBy1` comma

--selector = selectorSequence `sepBy1` combinator



combinator = lexeme $ oneOf "+>~ "

-- ClassSelector
classSelector = do
    char '.'
    i <- identifier
    return (ClassSelector i)

-- IDSelector
idSelector = do
    char '#'
    i <- identifier
    return (IDSelector i)

-- AttributeSelector
--attributeSelector = do
--    symbol "["
--    i <- lexeme identifier
    
--    char ']'

