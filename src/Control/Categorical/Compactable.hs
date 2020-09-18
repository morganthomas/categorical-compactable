{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}

module Control.Categorical.Compactable where

import Control.Category
import Data.Maybe

class Category a => Compactable a f where
  compact :: a (f (Maybe b)) (f b)
