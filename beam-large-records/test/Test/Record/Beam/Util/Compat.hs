{-# LANGUAGE MultiParamTypeClasses #-}

module Test.Record.Beam.Util.Compat (
    countAll_
  ) where

import Data.Int
import Database.Beam.Backend.SQL
import Database.Beam.Query.Internal

-- This provides compatibility between beam 0.8 (where this returns Int)
-- and beam 0.9 (where this is polymorphic in the result type, but must be
-- instantiated to a type with an explicit bitwidth)
countAll_ :: BeamSqlBackend be => QAgg be s Int32
countAll_ = QExpr (pure countAllE)