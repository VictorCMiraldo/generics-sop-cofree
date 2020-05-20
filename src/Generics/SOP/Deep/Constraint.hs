{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
-- |Provides type-level list membership and a few
-- niceties to work with compound constraints.
module Generics.SOP.Deep.Constraint where

import Data.Proxy
import Data.SOP.Constraint
import Data.SOP.Dict

-- | `elem` at the type level.
type family IsElem (a :: k) (as :: [ k ]) :: Bool where
  IsElem a (a ': as) = 'True
  IsElem a (b ': as) = IsElem a as
  IsElem a '[]       = 'False

-- | An actual proof that something is an element
data ElemPrf a as where
  Here  :: ElemPrf a (a ': as)
  There :: ElemPrf a as -> ElemPrf a (b ': as)

class HasElem a as where
  hasElem :: ElemPrf a as
instance {-# OVERLAPPING #-} HasElem a (a ': as) where
  hasElem = Here
instance {-# OVERLAPPABLE #-}
    (HasElem a as) => HasElem a (b ': as) where
  hasElem = There hasElem

-- |We will carry constructive information on the
-- constraint. Forcing 'IsElem' to true 
type Elem    a as = (IsElem a as ~ 'True , HasElem a as)

-- |Negation of 'Elem'
type NotElem a as = IsElem a as ~ 'False

-- |Provides the witness that @x@ is an instance of @c@
witness :: forall x xs c . (HasElem x xs , All c xs) => Proxy xs -> Dict c x
witness _ = witnessPrf (hasElem :: ElemPrf x xs)

-- |Constructs a 'Dict' for a specific element @x@ of @xs@
witnessPrf :: (All c xs) => ElemPrf x xs -> Dict c x
witnessPrf Here      = Dict
witnessPrf (There p) = witnessPrf p

-- |Example of using 'witness' to fetch the 'Eq' instance
-- for an element of a list
weq :: forall x xs . (All Eq xs , Elem x xs) => Proxy xs -> x -> x -> Bool
weq p = case witness p :: Dict Eq x of
            Dict -> (==)



