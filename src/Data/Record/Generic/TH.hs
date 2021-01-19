module Data.Record.Generic.TH (
    largeRecord
  ) where

import Language.Haskell.TH

largeRecord :: Q Type -> Q [Dec]
largeRecord qTyp = do
    typ <- qTyp
    runIO $ print typ
    return []
