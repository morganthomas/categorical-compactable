{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}

module Control.Categorical.Compactable where

import Control.Arrow
import Control.Categorical.Functor
import Control.Category
import Data.Either
import Data.Maybe
import Data.Tuple
import Prelude (Bool)

class Category a => Compactable a f where
  compact :: a (f (Maybe b)) (f b)
  default compact :: Arrow a => Functor f (->) a => a (f (Maybe b)) (f b)
  compact = separate . fmap (\case Just x -> Right x; _ -> Left ())
            >>> arr snd

  separate :: a (f (Either l r)) (f l, f r)
  default separate :: Arrow a => Functor f (->) a => a (f (Either l r)) (f l, f r)
  separate = (fmap flipEither >>> fmap hush >>> compact)
         &&& (fmap hush >>> compact)

  filter :: (b -> Bool) -> a (f b) (f b)
  default filter :: Functor f (->) a => (b -> Bool) -> a (f b) (f b)
  filter f = fmapMaybe (\x -> if f x then Just x else Nothing)

  partition :: (b -> Bool) -> a (f b) (f b, f b)
  default partition :: Functor f (->) a => (b -> Bool) -> a (f b) (f b, f b)
  partition f = fmapEither (\x -> if f x then Right x else Left x)

  fmapMaybe :: Functor f b a => b c (Maybe d) -> a (f c) (f d)
  fmapMaybe f = fmap f >>> compact

  fmapEither :: Functor f b a => b c (Either l r) -> a (f c) (f l, f r)
  fmapEither f = fmap f >>> separate

flipEither :: Either a b -> Either b a
flipEither = \case (Right x) -> Left x; (Left x) -> Right x;

hush :: Either l r -> Maybe r
hush = \case (Right x) -> Just x; _ -> Nothing
