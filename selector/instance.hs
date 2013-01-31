module Selector.Instance where

import Selector.Type

instance Show Selector where
    show (Sequence simpleSelectors) = concat . fmap show $ simpleSelectors
    show (simpleSelectors `Descendent` restSelectors) = (concat . fmap show $ simpleSelectors) ++ " " ++ show restSelectors
    show (simpleSelectors `Child` restSelectors) = (concat . fmap show $ simpleSelectors) ++ " > " ++ show restSelectors
    show (simpleSelectors `Sibling` restSelectors) = (concat . fmap show $ simpleSelectors) ++ " ~ " ++ show restSelectors
    show (simpleSelectors `AdjacentSibling` restSelectors) = (concat . fmap show $ simpleSelectors) ++ " + " ++ show restSelectors

instance Show SimpleSelector where
    show (TypeSelector s) = s
    show (UniversalSelector) = ""
    show (IDSelector s) = '#' : s
    show (ClassSelector s) = '.' : s
    show (AttributeSelector s op t) = "[" ++ s ++ show op ++ t ++ "]"
    show (PsuedoClassSelector s (PsuedoClassSelectorExpressionIdentifier t))    = ":" ++ s ++ "(" ++ t ++ ")"
    show (PsuedoClassSelector s (PsuedoClassSelectorExpressionOdd))             = ":" ++ s ++ "(odd)"
    show (PsuedoClassSelector s (PsuedoClassSelectorExpressionEven))            = ":" ++ s ++ "(even)"
    show (PsuedoClassSelector s (PsuedoClassSelectorExpressionValue a b))       = ":" ++ s ++ "(" ++ show a ++ "n+" ++ show b ++ ")"
    show (PsuedoClassSelector s (PsuedoClassSelectorExpressionNothing))         = ":" ++ s ++ "()"

instance Show AttributeOperator where
    show Equal = "="
    show Includes = "~="
    show PrefixMatch = "^="
    show SuffixMatch = "$="
    show SubstringMatch = "*="
    show DashMatch = "|="
    show Nop = ""
