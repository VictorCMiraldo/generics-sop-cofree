{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE PatternSynonyms       #-}
module Generics.SOP.Deep where

import qualified GHC.Generics as GHC
import           Generics.SOP
-----------------------------------
import Generics.SOP.Deep.Constraint

-- |A type @a@ is said to be primitive if it's
-- an element of the primitive types @kappa@ but not
-- of the compount types @fam@.
type PrimCnstr kappa fam a
  = (Elem a kappa , NotElem a fam)

-- |A type @a@ is said to be compount if its not primitive
-- and, additionally, it has a 'Generic' instance.
type CompoundCnstr kappa fam a
  = (Elem a fam , NotElem a kappa , Generic a)

-- |'HolesAnn' below is a cofree comonad on @ann@ and a free monad on @h@.
-- This allows us to use the same recursion operator for everything.
data HolesAnn kappa fam ann h a where
  Hole' :: ann a -- ^ Annotation
        -> h a -> HolesAnn kappa fam ann h a

  Prim' :: (PrimCnstr kappa fam a)
        => ann a -- ^ Annotation
        -> a -> HolesAnn kappa fam ann h a

  Roll' :: (CompoundCnstr kappa fam a)
        => ann a -- ^ Annotation
        -> SOP (HolesAnn kappa fam ann h) (Code a)
        -> HolesAnn kappa fam ann h a

-- |Deep representations are easily achieved by forbiding
-- the 'Hole'' constructor and providing unit annotations.
type Fix kappa fam = HolesAnn kappa fam GHC.U1 GHC.V1

pattern Fix :: () => (CompoundCnstr kappa fam a)
             => SOP (Fix kappa fam) (Code a)
             -> Fix kappa fam a
pattern Fix x = Roll x
{-# COMPLETE Fix , Prim #-}

-- |A tree with holes has unit annotations
type Holes kappa fam = HolesAnn kappa fam GHC.U1

pattern Hole :: h a -> Holes kappa fam h a
pattern Hole x = Hole' GHC.U1 x

pattern Prim :: () => (PrimCnstr kappa fam a)
             => a -> Holes kappa fam h a
pattern Prim a = Prim' GHC.U1 a

pattern Roll :: () => (CompoundCnstr kappa fam a)
             => SOP (Holes kappa fam h) (Code a)
             -> Holes kappa fam h a
pattern Roll x = Roll' GHC.U1 x
{-# COMPLETE Hole , Prim , Roll #-}

-- |Annotated fixpoints are also easy; forbid the 'Hole''
-- constructor but add something to every 'Roll' of
-- the representation.
type FixAnn kappa fam ann = HolesAnn kappa fam ann GHC.V1

pattern PrimAnn :: () => (PrimCnstr kappa fam a)
                => ann a -> a -> FixAnn kappa fam ann a
pattern PrimAnn ann a = Prim' ann a

pattern FixAnn :: () => (CompoundCnstr kappa fam a)
                => ann a
                -> SOP (FixAnn kappa fam ann) (Code a)
                -> FixAnn kappa fam ann a
pattern FixAnn ann x = Roll' ann x
{-# COMPLETE FixAnn , PrimAnn #-}

-------------------------
-- * Deep Conversion * --
-------------------------
--
-- $deepconv
--
-- Converting values to and from their deep representations
-- is done with 'dfrom' and 'dto'. The 'GDeep' and 'GDeep''
-- typeclasses are auxiliary and can be ignored by users.
-- All datatypes that have a 'Generic' instance have a 'GDeep'
-- instance too, hence the users can use 'dfrom' and 'dto'
-- without hassle. See "Generics.SOP.Example" for details.

-- |Converts a value to its deep representation.
dfrom :: forall kappa fam a
       . ( CompoundCnstr kappa fam a , All2 (GDeep kappa fam) (Code a))
      => a -> Fix kappa fam a
dfrom = Fix . hcmap (Proxy :: Proxy (GDeep kappa fam)) (gdfrom . unI) . from

-- |Converts a value from its deep representation.
dto :: forall kappa fam a
     . ( CompoundCnstr kappa fam a , All2 (GDeep kappa fam) (Code a))
    => Fix kappa fam a -> a
dto (Fix f) = to $ hcmap (Proxy :: Proxy (GDeep kappa fam)) (I . gdto) f


-- TODO: refactor these classes to an internal module?

-- |Use 'dfrom' and 'dto' directly; this typeclass is internal
class GDeep kappa fam a where
  gdfrom :: a -> Fix kappa fam a
  gdto   :: Fix kappa fam a -> a

instance GDeep' kappa fam (IsElem a kappa) a => GDeep kappa fam a where
  gdfrom = gdfrom' (Proxy :: Proxy (IsElem a kappa))
  gdto   = gdto'   (Proxy :: Proxy (IsElem a kappa))

-- |The @GDeep'@ class is somewhat of a hack. It carries a (type-level)
-- boolean parameter dictating whether we are converting a primitive
-- type or a compound one and this dictates the choice of instance. 
class GDeep' kappa fam isPrim a where
  gdfrom'  :: Proxy isPrim -> a -> Fix kappa fam a
  gdto'    :: Proxy isPrim -> Fix kappa fam a -> a

instance (All2 (GDeep kappa fam) (Code a) , CompoundCnstr kappa fam a)
     => GDeep' kappa fam 'False a where
  gdfrom' _ a = dfrom a
  gdto' _   x = dto x

instance (PrimCnstr kappa fam a) => GDeep' kappa fam 'True a where
  gdfrom' _ a        = Prim a
  gdto'   _ (Prim a) = a


-- TODO: maps; folds; zips and all the shenanigans!
