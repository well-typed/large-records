{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Data.Record.Beam.Skeleton (
    GLargeTableSkeleton
  , TblSkeletonI
  ) where

import Data.Proxy
import Data.Record.Generic
import Data.Record.Generic.GHC
import Data.Record.Generic.Transform
import Database.Beam.Schema.Tables
import GHC.Generics hiding (Generic(..))

import qualified Data.Record.Generic.Rep as Rep

import Data.Record.Beam.Interpretation

type GLargeTableSkeleton tbl = (
    Generic (tbl Ignored)
  , Generic (tbl Uninterpreted)
  , Constraints (tbl Uninterpreted) TblSkeletonI
  , HasNormalForm (BeamInterpretation Ignored) (tbl Ignored) (tbl Uninterpreted)
  )

instance GLargeTableSkeleton tbl
      => GTableSkeleton (ThroughLRGenerics (tbl Ignored)) where
  gTblSkeleton _ =
    WrapThroughLRGenerics . to . denormalize1 (Proxy @BeamInterpretation) $
      Rep.cpure (Proxy @TblSkeletonI) tblSkeletonI

{-------------------------------------------------------------------------------
  Cases for 'gTblSkeleton'

  The 'BeamInterpretation' makes it possible to mirror the case distinction
  that beam is using in the 'GTableSkeleton' instances.
-------------------------------------------------------------------------------}

class TblSkeletonI a where
  tblSkeletonI :: Interpret (BeamInterpretation Ignored) a

instance TblSkeletonI (Uninterpreted x) where
  tblSkeletonI = Interpret $ unK1 fromBeam
    where
      fromBeam :: K1 R (Ignored field) ()
      fromBeam = gTblSkeleton Proxy

instance Beamable tbl => TblSkeletonI (tbl Uninterpreted) where
  tblSkeletonI = Interpret $ unK1 fromBeam
    where
      fromBeam :: K1 R (tbl Ignored) ()
      fromBeam = gTblSkeleton Proxy

instance Beamable tbl => TblSkeletonI (tbl (Nullable Uninterpreted)) where
  tblSkeletonI = Interpret $ unK1 fromBeam
    where
      fromBeam :: K1 R (tbl (Nullable Ignored)) ()
      fromBeam = gTblSkeleton Proxy
