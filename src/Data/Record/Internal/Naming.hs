{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}

-- | Names of the various things we generate
--
-- This is used by both TH code generation and the quasi-quoter.
module Data.Record.Internal.Naming (
    -- * Names based on the constructor
    nameRecordInternalConstr
  , nameRecordTypedConstructorFn
    -- * Names based on the type
  , nameRecordConstraintsClass
  , nameRecordConstraintsMethod
  , nameRecordIndexedAccessor
  , nameRecordIndexedOverwrite
  , nameRecordInternalField
  , nameRecordView
  ) where

{-------------------------------------------------------------------------------
  Names based on the constructor
-------------------------------------------------------------------------------}

-- | The name of the constructor used internally
--
-- We must pick this so that
--
-- 1. It is different from the user-written constructor (so that we can use that
--    name for the pattern synonym, /if/ we generate it)
--
-- 2. It is derivable /from/ the user-written constructor, so that in, say,
--
--    > [lr| MkR { x = 5, y = True } |]
--
--    the quasi-quoter can figure out the name of the internal constructor
--    (provided that the constructor is in scope, but that's a reasonable
--    requirement).
nameRecordInternalConstr :: String -> String
nameRecordInternalConstr = ("LR__" ++)

-- | Name of the record constructor function
--
-- Unlike the internal constructor (which takes a @Vector Any@ as argument),
-- this function takes @n@ arguments, one for each record field, of the
-- appropriate types.
nameRecordTypedConstructorFn :: String -> String
nameRecordTypedConstructorFn = ("_construct_" ++)

{-------------------------------------------------------------------------------
  Names based on the type
-------------------------------------------------------------------------------}

nameRecordConstraintsClass  :: String -> String
nameRecordConstraintsMethod :: String -> String
nameRecordIndexedAccessor   :: String -> String
nameRecordIndexedOverwrite  :: String -> String
nameRecordInternalField     :: String -> String
nameRecordView              :: String -> String

nameRecordConstraintsClass  = ("Constraints_"     ++)
nameRecordConstraintsMethod = ("dictConstraints_" ++)
nameRecordIndexedAccessor   = ("unsafeGetIndex"   ++)
nameRecordIndexedOverwrite  = ("unsafeSetIndex"   ++)
nameRecordInternalField     = ("vectorFrom"       ++)
nameRecordView              = ("tupleFrom"        ++)
