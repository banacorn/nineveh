module Selector.Type where

-- a selector is a series of simple selector sequences seperated by combinators
data Selector   = Sequence [SimpleSelector]
                | [SimpleSelector] `Descendent` Selector
                | [SimpleSelector] `Child` Selector
                | [SimpleSelector] `AdjacentSibling` Selector 
                | [SimpleSelector] `Sibling` Selector 
                deriving (Eq)

data SimpleSelector = TypeSelector String 
                    | UniversalSelector
                    | IDSelector String
                    | ClassSelector String 
                    | AttributeSelector String AttributeOperator String
                    | PsuedoClassSelector String PsuedoClassSelectorExpression
                    | NegationSelector SimpleSelector
                    deriving (Eq)

-- some ugly shit for the [a~=b] stuff
data AttributeOperator = Equal | Includes | PrefixMatch | SuffixMatch | SubstringMatch | DashMatch | Nop deriving (Eq)

-- fuck this is ugly
data PsuedoClassSelectorExpression  = PsuedoClassSelectorExpressionIdentifier String
                                    | PsuedoClassSelectorExpressionOdd
                                    | PsuedoClassSelectorExpressionEven
                                    | PsuedoClassSelectorExpressionValue Integer Integer
                                    | PsuedoClassSelectorExpressionNothing
                                    deriving (Eq, Show)
