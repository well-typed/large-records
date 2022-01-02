-- | Options that influence TH code generation
module Data.Record.TH.Config.Options (
    Options(..)
  , defaultStrictOptions
  , defaultLazyOptions
  , defaultPureScript
  ) where

{-------------------------------------------------------------------------------
  Options
-------------------------------------------------------------------------------}

-- | Tweak the output of the generator
--
-- In the explanations of the various options below, we will use the following
-- record as our running example:
--
-- > data T a b = MkT {
-- >       tWord  :: Word
-- >     , tBool  :: Bool
-- >     , tChar  :: Char
-- >     , tA     :: a
-- >     , tListB :: [b]
-- >     }
-- >   deriving (Eq, Show)
data Options = Options {
      -- | Generate 'HasField' instances
      --
      -- > instance HasField "tInt" (T a b) Word where
      -- >   hasField = ..
      --
      -- These are required by the @record-dot-preprocessor@.
      generateHasFieldInstances :: Bool

      -- | Make all fields strict
      --
      -- This should be used when using the @StrictData@ or @Strict@ language
      -- extension.
    , allFieldsStrict :: Bool
    }

{-------------------------------------------------------------------------------
  Defaults
-------------------------------------------------------------------------------}

defaultLazyOptions :: Options
defaultLazyOptions = Options {
      generateHasFieldInstances = True
    , allFieldsStrict           = False
    }

defaultStrictOptions :: Options
defaultStrictOptions = Options {
      generateHasFieldInstances = True
    , allFieldsStrict           = True
    }

-- | Default options for "Purescript style" records
--
-- This is currently equal to 'defaultStrictOptions'.
--
-- NOTE: The @record-dot-preprocessor@ enables @DuplicateRecordFields@ by
-- default. Since the records that we produce are not visible to @ghc@,
-- @large-records@ is not compatible with DRF-style overloading. However, as
-- long as all overloading is resolved through @HasField@ instead (which is
-- what @record-dot-preprocessor@ encourages anyway), all is fine.
defaultPureScript :: Options
defaultPureScript = Options {
      generateHasFieldInstances = True
    , allFieldsStrict           = True
    }
