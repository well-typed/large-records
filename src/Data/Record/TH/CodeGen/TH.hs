{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Utility functions for working with TH
module Data.Record.TH.CodeGen.TH (
    -- * Folding
    appsT
  , arrT
    -- * Constructing lists (variations on 'listE')
  , vectorE
  , plistT
  , ptupleT
    -- * Simplified construction
  , simpleFn
  , simplePatSynType
    -- * Dealing with type variables
  , tyVarName
  , tyVarType
    -- * Bang
  , pattern DefaultBang
    -- * Extensions
  , requiresExtensions
  ) where

import Control.Monad
import Data.List (intercalate)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import qualified Data.Vector as V

import qualified Data.Record.TH.CodeGen.Name as N

{-------------------------------------------------------------------------------
  Folding
-------------------------------------------------------------------------------}

-- | Repeated application
--
-- @appsT f [x1, .., xN]@ constructs something like
--
-- > f x1 .. xN
appsT :: Q Type -> [Q Type] -> Q Type
appsT t ts = foldl appT t ts

-- | Repeated application of @(->)@
--
-- @arrT [x1, .., xN] y@ constructs something like
--
-- > x1 -> .. -> xN -> y
arrT :: [Q Type] -> Q Type -> Q Type
arrT ts t = foldr (\a b -> arrowT `appT` a `appT` b) t ts

{-------------------------------------------------------------------------------
  Constructing lists (variations on 'listE')
-------------------------------------------------------------------------------}

vectorE :: (a -> Q Exp) -> [a] -> Q Exp
vectorE f elems = [| V.fromList $(listE (map f elems)) |]

plistT :: [Q Type] -> Q Type
plistT = foldr cons nil
  where
    nil       = promotedNilT
    cons t ts = promotedConsT `appT` t `appT` ts

ptupleT :: [Q Type] -> Q Type
ptupleT ts = appsT (promotedTupleT (length ts)) ts

{-------------------------------------------------------------------------------
  Simplified construction
-------------------------------------------------------------------------------}

-- | Construct simple function
--
-- @simpleFn n typ body@ constructs something like
--
-- > f :: typ
-- > f = body
simpleFn :: N.Name 'N.Dynamic -> Q Type -> Q Exp -> Q [Dec]
simpleFn fnName qTyp qBody = do
    typ  <- qTyp
    body <- qBody
    return [
          SigD fnName' typ
        , ValD (VarP fnName') (NormalB body) []
        ]
  where
    fnName' :: Name
    fnName' = N.toName fnName

-- | Construct simple pattern synonym type
--
-- @simplePatSynType xs [t1, .., tn] s@ constructs something like
--
-- > pattern foo :: forall xs. t1 -> .. -> tn -> s
simplePatSynType :: [TyVarBndr] -> [Q Type] -> Q Type -> Q PatSynType
simplePatSynType tvars fieldTypes resultType =
      forallT tvars (cxt [])
    $ forallT []    (cxt [])
    $ arrT fieldTypes resultType

{-------------------------------------------------------------------------------
  Dealing with type variables
-------------------------------------------------------------------------------}

tyVarName :: TyVarBndr -> Name
tyVarName (PlainTV  n)   = n
tyVarName (KindedTV n _) = n

tyVarType :: TyVarBndr -> Q Type
tyVarType = varT . tyVarName

{-------------------------------------------------------------------------------
  Bang
-------------------------------------------------------------------------------}

pattern DefaultBang :: Bang
pattern DefaultBang = Bang NoSourceUnpackedness NoSourceStrictness

{-------------------------------------------------------------------------------
  Extensions
-------------------------------------------------------------------------------}

-- | Check that the specified extensions are enabled
--
-- To improve user experience, we report all missing extensions at once (rather
-- than giving an error for the first missing one).
requiresExtensions :: Quasi m => [Extension] -> m ()
requiresExtensions exts = runQ $ do
    disabled <- filterM (fmap not . isExtEnabled) exts
    unless (null disabled) $ do
      fail $ "Please enable " ++ intercalate ", " (map show disabled)
