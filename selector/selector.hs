-- Reference: Selectors Level 3 http://www.w3.org/TR/css3-selectors/

module Selector (parseSelector) where

import Control.Applicative ((<$>))
import Data.List (intercalate)
import Text.ParserCombinators.Parsec
import Parser
import Tokenize

-- a selector is a series of simple selector sequences seperated by combinators
data Selector   = Sequence [SimpleSelector]
                | [SimpleSelector] `Descendent` Selector
                | [SimpleSelector] `Child` Selector
                | [SimpleSelector] `AdjacentSibling` Selector 
                | [SimpleSelector] `Sibling` Selector 
                deriving (Eq, Show)

data SimpleSelector = TypeSelector String 
                    | UniversalSelector
                    | IDSelector String
                    | ClassSelector String 
                    | AttributeSelector String AttributeOperator String
                    | PsuedoClassSelector String PsuedoClassSelectorExpression
                    | NegationSelector SimpleSelector
                    deriving (Eq, Show)

-- some ugly shit for the [a~=b] stuff
data AttributeOperator = Equal | Includes | PrefixMatch | SuffixMatch | SubstringMatch | DashMatch | Nop deriving (Eq, Show)

-- fuck this is ugly
data PsuedoClassSelectorExpression  = PsuedoClassSelectorExpressionIdentifier String
                                    | PsuedoClassSelectorExpressionOdd
                                    | PsuedoClassSelectorExpressionEven
                                    | PsuedoClassSelectorExpressionValue Integer Integer
                                    | PsuedoClassSelectorExpressionNothing
                                    deriving (Eq, Show)


parseSelector = try descendentCase <|> try otherCases <|> lastSequence
    where   descendentCase = do
                sequence <- lexeme selectorSequence
                restSequences <- parseSelector
                return (sequence `Descendent` restSequences)
            otherCases = do 
                sequence <- lexeme selectorSequence
                c <- combinator
                restSequences <- parseSelector
                case c of
                    '+' -> return (sequence `AdjacentSibling` restSequences)
                    '~' -> return (sequence `Sibling` restSequences)
                    '>' -> return (sequence `Child` restSequences)
            lastSequence = lexeme selectorSequence >>= return . Sequence    

selectorSequence = 
    do
        h <- typeSelector <|> universalSelector
        hs <- many (idSelector <|> classSelector <|> attributeSelector <|> try negationSelector <|> psuedoClassSelector)
        return (h:hs)
    <|> do
        hs <- many1 (idSelector <|> classSelector <|> attributeSelector <|> try negationSelector <|> psuedoClassSelector)
        return (UniversalSelector:hs)

combinator = lexeme $ oneOf "+>~ "

--
--  Simple Selectors                  
--

typeSelector = identifier >>= return . TypeSelector

universalSelector = symbol "*" >> return UniversalSelector

-- ClassSelector
classSelector = char '.' >> identifier >>= return . ClassSelector

-- IDSelector
idSelector = char '#' >> identifier >>= return . IDSelector

-- AttributeSelector
attributeSelector = do
    symbol "["
    i <- lexeme identifier
    (o, p) <- option (Nop, "") (do
        operator <- lexeme attributeOperator
        operand <- lexeme identifier <|> lexeme string'
        return (operator, operand))
    char ']'
    return (AttributeSelector i o p)

attributeOperator :: Parser AttributeOperator
attributeOperator    =  (string "=" >> return Equal)
                    <|> (string "~=" >> return Includes)
                    <|> (string "^=" >> return PrefixMatch)
                    <|> (string "$=" >> return SuffixMatch)
                    <|> (string "*=" >> return SubstringMatch)
                    <|> (string "|=" >> return DashMatch)

-- PsuedoClassSelector
psuedoClassSelector :: Parser SimpleSelector
psuedoClassSelector = do
    try (string "::") <|> string ":"
    try (do 
            (i, s) <- function psuedoClassSelectorExpression
            return (PsuedoClassSelector i s)
        ) <|> (do
            i <- identifier
            return (PsuedoClassSelector i PsuedoClassSelectorExpressionNothing)
        )

psuedoClassSelectorExpression :: Parser PsuedoClassSelectorExpression
psuedoClassSelectorExpression    =  psuedoClassSelectorExpressionValue 
                                <|> psuedoClassSelectorExpressionEven
                                <|> psuedoClassSelectorExpressionOdd
                                <|> psuedoClassSelectorExpressionIdentifier 

psuedoClassSelectorExpressionValue = try linear <|> constant 
    where
        -- an + b
        linear = do
            d <- option 1 sign
            r <- option 0 integer
            symbol "n"
            (d', r') <- option (1, 0) (do
                e <- lexeme sign    
                s <- integer
                return (e, s))
            return (PsuedoClassSelectorExpressionValue (d * r) (d' * r'))
        -- b
        constant = do
            d <- option 1 sign
            r <- integer
            return (PsuedoClassSelectorExpressionValue 0 (d * r))

        sign = (string "+" >> return 1) <|> (string "-" >> return (-1))

psuedoClassSelectorExpressionIdentifier = lexeme identifier >>= return . PsuedoClassSelectorExpressionIdentifier
psuedoClassSelectorExpressionEven       = symbol "even"     >>  return   PsuedoClassSelectorExpressionEven
psuedoClassSelectorExpressionOdd        = symbol "odd"      >>  return   PsuedoClassSelectorExpressionOdd

-- NegationSelector
negationSelector = do
    string ":not"
    parens (lexeme negationArgument) >>= return . NegationSelector

negationArgument :: Parser SimpleSelector
negationArgument =  typeSelector 
                <|> universalSelector 
                <|> idSelector
                <|> classSelector 
                <|> psuedoClassSelector 
                <|> attributeSelector