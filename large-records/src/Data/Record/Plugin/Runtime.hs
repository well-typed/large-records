{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeApplications         #-}

-- | Re-exports of types and functions used by generated code
--
-- This exports all functionality required by the generated code, with the
-- exception of GHC generics (name clash with @large-records@ generics).
module Data.Record.Plugin.Runtime (
    -- * Base
    Constraint
  , Proxy
  , Type
  , proxy
    -- * AnyArray
  , AnyArray
  , anyArrayFromList
  , anyArrayToList
  , anyArrayIndex
  , anyArrayUpdate
    -- * large-generics
  , Rep
  , Dict
  , anyArrayToRep
  , anyArrayFromRep
  , mkDicts
  , mkDict
  , mkStrictField
  , mkLazyField
  , mkMetadata
    -- ** wrappers
  , gcompare
  , geq
  , gshowsPrec
  , noInlineUnsafeCo
    -- ** ThroughLRGenerics
  , ThroughLRGenerics
  , wrapThroughLRGenerics
  , unwrapThroughLRGenerics
  ) where

import Control.Monad (forM_)
import Data.Coerce (coerce)
import Data.Primitive.SmallArray
import GHC.Exts (Any)
import GHC.TypeLits

import qualified Data.Foldable                    as Foldable
import qualified Data.Kind                        as Base
import qualified Data.Proxy                       as Base
import qualified Data.Record.Generic              as LR
import qualified Data.Record.Generic.Eq           as LR
import qualified Data.Record.Generic.GHC          as LR
import qualified Data.Record.Generic.Rep.Internal as LR
import qualified Data.Record.Generic.Show         as LR

{-------------------------------------------------------------------------------
  base
-------------------------------------------------------------------------------}

type Constraint = Base.Constraint
type Proxy      = Base.Proxy
type Type       = Base.Type

proxy :: forall k (a :: k). Proxy a
proxy = Base.Proxy

{-------------------------------------------------------------------------------
  AnyArray
-------------------------------------------------------------------------------}

type AnyArray = SmallArray Any

anyArrayFromList :: [Any] -> AnyArray
anyArrayFromList = smallArrayFromList

anyArrayToList :: AnyArray -> [Any]
anyArrayToList = Foldable.toList

anyArrayIndex :: AnyArray -> Int -> Any
anyArrayIndex = indexSmallArray

anyArrayUpdate :: AnyArray -> [(Int, Any)] -> AnyArray
anyArrayUpdate v updates = runSmallArray $ do
    v' <- thawSmallArray v 0 (sizeofSmallArray v)
    forM_ updates $ \(i, a) -> do
      writeSmallArray v' i a
    return v'

{-------------------------------------------------------------------------------
  large-generics: utilities
-------------------------------------------------------------------------------}

anyArrayToRep :: AnyArray -> Rep LR.I a
anyArrayToRep = coerce

anyArrayFromRep :: Rep LR.I a -> AnyArray
anyArrayFromRep = coerce

mkDicts :: [Dict c Any] -> Rep (Dict c) a
mkDicts = LR.Rep . smallArrayFromList

mkDict :: c x => Proxy c -> Proxy x -> Dict c x
mkDict _ _ = LR.Dict

mkStrictField :: forall name a.
     KnownSymbol name
  => Proxy name -> LR.FieldMetadata a
mkStrictField _ = LR.FieldMetadata (Base.Proxy @name) LR.FieldStrict

mkLazyField :: forall name a.
     KnownSymbol name
  => Proxy name -> LR.FieldMetadata a
mkLazyField _ = LR.FieldMetadata (Base.Proxy @name) LR.FieldLazy

mkMetadata ::
     String  -- ^ Record name
  -> String  -- ^ Constructor name
  -> [LR.FieldMetadata Any]
  -> LR.Metadata a
mkMetadata name constr fields = LR.Metadata {
      recordName          = name
    , recordConstructor   = constr
    , recordSize          = length fields
    , recordFieldMetadata = LR.Rep $ smallArrayFromList fields
    }

{-------------------------------------------------------------------------------
  large-generics: wrappers
-------------------------------------------------------------------------------}

type Rep  = LR.Rep
type Dict = LR.Dict

gcompare :: (LR.Generic a, LR.Constraints a Ord) => a -> a -> Ordering
gcompare = LR.gcompare

geq :: (LR.Generic a, LR.Constraints a Eq) => a -> a -> Bool
geq = LR.geq

gshowsPrec :: (LR.Generic a, LR.Constraints a Show) => Int -> a -> ShowS
gshowsPrec = LR.gshowsPrec

noInlineUnsafeCo :: a -> b
noInlineUnsafeCo = LR.noInlineUnsafeCo

{-------------------------------------------------------------------------------
  large-generics: ThroughLRGenerics
-------------------------------------------------------------------------------}

type ThroughLRGenerics = LR.ThroughLRGenerics

wrapThroughLRGenerics :: a -> ThroughLRGenerics a p
wrapThroughLRGenerics = LR.WrapThroughLRGenerics

unwrapThroughLRGenerics :: ThroughLRGenerics a p -> a
unwrapThroughLRGenerics = LR.unwrapThroughLRGenerics
