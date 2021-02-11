{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Thin layer on top of 'Name'
--
-- TODO: We should not be working with unqualified names.
module Data.Record.TH.CodeGen.Name (
    -- * Different types of names
    TypeName(..)
  , ConstrName(..)
  , FieldName(..)
    -- * Operations that work on any kind of name
  , IsName -- opaque
  , fresh
  , name
  , nameWithSuffix
  , nameWithPrefix
  , nameExpr
  , nameType
  ) where

import Data.Coerce
import Language.Haskell.TH

{-------------------------------------------------------------------------------
  Different types of names
-------------------------------------------------------------------------------}

newtype TypeName = TypeName String
  deriving newtype  (Show, Eq, Ord)
  deriving anyclass IsName

newtype ConstrName = ConstrName String
  deriving newtype  (Show, Eq, Ord)
  deriving anyclass IsName

newtype FieldName = FieldName  String
  deriving newtype  (Show, Eq, Ord)
  deriving anyclass IsName

{-------------------------------------------------------------------------------
  Operations that work on any kind of name
-------------------------------------------------------------------------------}

class Coercible n String => IsName n where

fresh :: IsName n => n -> Q Name
fresh = newName . coerce

name :: IsName n => n -> Name
name = nameWithSuffix ""

nameWithSuffix :: IsName n => String -> n -> Name
nameWithSuffix suffix = mkName . (++ suffix) . coerce

nameWithPrefix :: IsName n => String -> n -> Name
nameWithPrefix prefix = mkName . (prefix ++) . coerce

nameExpr :: IsName n => n -> Q Exp
nameExpr = litE . stringL . coerce

nameType :: IsName n => n -> Q Type
nameType = litT . strTyLit . coerce
