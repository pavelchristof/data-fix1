{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{- |
Module      :  Data.Fix1
Description :  Fixed point for types of kind (k -> *) -> k -> *
Copyright   :  (c) Paweł Nowak
License     :  MIT

Maintainer  :  pawel834@gmail.com
Stability   :  experimental
-}
module Data.Fix1 where

import Prelude.Compat

-- | This is similar to an "endofunctor on the category of endofunctors", but
-- the forall is lifted outside.
--
-- I speculate that this hmap is implementable for exactly the same types as
-- a hmap of type @(f ~> g) -> (h f ~> h g)@, yet allows more uses.
class HFunctor (h :: (k -> *) -> (k -> *)) where
    hmap :: (f a -> g a) -> (h f a -> h g a)

-- | Types such that @g f@ is a functor given that @f@ is a functor.
class Functor1 (g :: (* -> *) -> (* -> *)) where
    map1 :: Functor f => (a -> b) -> (g f a -> g f b)

-- | Types such that @g f@ is foldable given that @f@ is foldable.
class Foldable1 (g :: (* -> *) -> (* -> *)) where
    foldMap1 :: (Foldable f, Monoid m) => (a -> m) -> (g f a -> m)

-- | Types such that @h f@ is traversable given that @f@ is traversable.
class (Functor1 h, Foldable1 h) => Traversable1 (h :: (* -> *) -> (* -> *)) where
    traverse1 :: (Traversable f, Applicative g) => (a -> g b) -> (h f a -> g (h f b))

-- | Fixed point of a type with kind (k -> *) -> k -> *
data Fix1 (f :: (k -> *) -> k -> *) (a :: k) = Fix1 { unFix1 :: f (Fix1 f) a }

instance Functor1 f => Functor (Fix1 f) where
    fmap f = Fix1 . map1 f . unFix1

instance Foldable1 f => Foldable (Fix1 f) where
    foldMap f = foldMap1 f . unFix1

instance Traversable1 f => Traversable (Fix1 f) where
    traverse f (Fix1 a) = Fix1 <$> traverse1 f a

cata1 :: HFunctor f => (f g a -> g a) -> Fix1 f a -> g a
cata1 f = f . hmap (cata1 f) . unFix1

ana1 :: HFunctor f => (g a -> f g a) -> g a -> Fix1 f a
ana1 f = Fix1 . hmap (ana1 f) . f

hylo1 :: HFunctor f => (f g a -> g a) -> (h a -> f h a) -> (h a -> g a)
hylo1 phi psi = cata1 phi . ana1 psi

infixr 9 .:
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)

infixr 9 .::
(.::) :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
(.::) = (.:) . (.)
