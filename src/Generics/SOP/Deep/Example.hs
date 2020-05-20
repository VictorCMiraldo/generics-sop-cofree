{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Generics.SOP.Deep.Example where

import           Data.Function (on)
import qualified GHC.Generics as GHC
import           Generics.SOP
------
import Generics.SOP.Deep
import Generics.SOP.Deep.Eq

-- The classic mutually recursive example (which is actually a nested type,
-- but that's fine!).
data Rose a = Fork a [Rose a]
  deriving (GHC.Generic)

type Prim = '[Int]
type Fam  = '[Rose Int , [Rose Int]]

-- We need a Generics.SOP 'Generic' instance, which is derived from
-- the GHC.Generics one. 
instance Generic (Rose a)

-- And this already enables us to use 'dfrom'. We need to instantiate
-- the typevariables manually so GHC knows which family and which primitive
-- types we are using.
dfromRose :: Rose Int -> Fix Prim Fam (Rose Int)
dfromRose = dfrom

-- Which in turn, enable us to write the Eq instance for Rose Int
instance Eq (Rose Int) where
  (==) = geq `on` dfromRose


-- Let's test it out with two trees.

t1 :: Rose Int
t1 = Fork 42 [ Fork 21 [Fork 10 [] , Fork 11 []]
             , Fork 21 [Fork 11 [] , Fork 10 []]]

t2 :: Rose Int
t2 = Fork 1 []

works :: Bool
works = and [t1 == t1 , t1 /= t2 , t2 == t2]

