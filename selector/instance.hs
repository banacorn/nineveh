module Selector.Instance where

import Selector.Type

instance Show Selector where
    show (Sequence simpleSelectors) = show simpleSelectors
    show (simpleSelectors `Descendent` restSelectors) = show simpleSelectors ++ " " ++ show restSelectors
    show (simpleSelectors `Child` restSelectors) = show simpleSelectors ++ " > " ++ show restSelectors
    show (simpleSelectors `Sibling` restSelectors) = show simpleSelectors ++ " ~ " ++ show restSelectors
    show (simpleSelectors `AdjacentSibling` restSelectors) = show simpleSelectors ++ " + " ++ show restSelectors