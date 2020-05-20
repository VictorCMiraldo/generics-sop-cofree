{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |Provides deep generic equality
module Generics.SOP.Deep.Eq where

import Generics.SOP
------
import Generics.SOP.Deep
import Generics.SOP.Deep.Constraint

-- |Unlike shallow generic equality, where we require that
-- @All2 Eq (Code a)@, since the whole value is already in
-- its generic form, all we need is equality for the primitive
-- types.
geq :: forall kappa fam a . (All Eq kappa)
    => Fix kappa fam a -> Fix kappa fam a -> Bool
geq (Prim x) (Prim y) = weq (Proxy :: Proxy kappa) x y
geq (Fix  f) (Fix  g) = ccompare_NS (Proxy :: Proxy SListI) False go False
                                    (unSOP f)
                                    (unSOP g)
  where
    go :: SListI x => NP (Fix kappa fam) x -> NP (Fix kappa fam) x -> Bool
    go xs ys = and $ hcollapse $ hzipWith (\x y -> K (geq x y)) xs ys
