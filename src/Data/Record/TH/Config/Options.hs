-- | Options that influence TH code generation
module Data.Record.TH.Config.Options (
    Options(..)
  , GenPatSynonym(..)
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
      -- | Generate a pattern synonym for the record
      --
      -- See 'GenPatSynonym' for details. Defaults to 'UseQuasiQuoter'.
      generatePatternSynonym :: GenPatSynonym

      -- | Generate a "constructor function" for the record
      --
      -- > _construct_MkT :: Word -> Bool -> Char -> a -> [b] -> T a b
      -- > _construct_MkT = ..
      --
      -- This function can be used directly, but it is also used by the 'lr'
      -- quasi-quoter, so if this function is not generated, 'lr' will not work.
    , generateConstructorFn :: Bool

      -- | Generate 'HasField' instances
      --
      -- > instance HasField "tInt" (T a b) Word where
      -- >   hasField = ..
      --
      -- These are required by the @record-dot-preprocessor@.
    , generateHasFieldInstances :: Bool

      -- | Generate field accessors
      --
      -- > tInt :: T a b -> Word
      -- > tInt = ..
      --
      -- If field accessors are not generated, the only way to access fields
      -- is through the 'HasField' instances.
      --
      -- Disabling this option is primarily useful if you need overloading:
      -- if you have multiple records with a field of the same name, then
      -- generating field accessors would result in name clashes. Without the
      -- accessors, overloading is resolved through 'HasField'.
    , generateFieldAccessors :: Bool

      -- | Make all fields strict
      --
      -- This should be used when using the @StrictData@ or @Strict@ language
      -- extension.
    , allFieldsStrict :: Bool
    }

-- | Do we want to generate a pattern synonym for the record?
data GenPatSynonym =
    -- | Generate pattern synonym
    --
    -- If generating a pattern synonym is enabled, 'largeRecord' will generate
    -- something like
    --
    -- > pattern MkT :: Word -> Bool -> Char -> a -> [b] -> T a b
    -- > pattern MkT{tInt, tBool, tChar, tA, tListB} <- ..
    -- >   where
    -- >     MkT tInt' tBool' tChar' tA' tListB' = ..
    --
    -- The pattern synonym makes it possible to construct or pattern match on
    -- @T@ values as if it had been defined like a normal record.
    --
    -- We do /not/ do this by default, however, because unfortunately when
    -- we define a record pattern synonym in @ghc@, @ghc@ also (unnecessarily
    -- but currently unavoidably) introduces field accessors for all fields
    -- in the record, and we're back to code that is quadratic in size. There
    -- is hope that this situation will be better from ghc 9.2 forward
    -- (new `NoFieldSelectors` extension) but it's unclear at present.
    --
    -- For now, avoid if possible (see 'UseQuasiQuoter').
    GenPatSynonym

    -- | Do not generate the pattern synonym
    --
    -- In the absence of a pattern synonym, the main way to interact with
    -- records is through the 'HasField' instances (i.e., by using lenses).
    -- However, it is sometimes still useful to be able to pattern match on
    -- records or construct explicit records values. The quasi-quoter makes
    -- this possible. It can be used in three ways:
    --
    -- o In pattern matches:
    --
    --   > foo :: T a b -> Word
    --   > foo [lr| MkT { tInt = x } |] = x
    --
    -- o In record construction:
    --
    --    > bar :: T () Double
    --    > bar = [lr| MkT {
    --    >       tInt   = 0
    --    >     , tBool  = False
    --    >     , tChar  = 'a'
    --    >     , tA     = ()
    --    >     , tListB = [0.0]
    --    >     } |]
    --
    -- o As a constructor:
    --
    --    > baz :: T () Double
    --    > baz = [lr| MkT |] 0 False 'a' () [0.0]
    --
    -- NOTE: The quasi-quoter can NOT be used if the pattern synonym is enabled.
  | UseQuasiQuoter
  deriving (Show, Eq)

{-------------------------------------------------------------------------------
  Defaults
-------------------------------------------------------------------------------}

defaultLazyOptions :: Options
defaultLazyOptions = Options {
      generatePatternSynonym    = UseQuasiQuoter
    , generateConstructorFn     = True
    , generateHasFieldInstances = True
    , generateFieldAccessors    = True
    , allFieldsStrict           = False
    }

defaultStrictOptions :: Options
defaultStrictOptions = Options {
      generatePatternSynonym    = UseQuasiQuoter
    , generateConstructorFn     = True
    , generateHasFieldInstances = True
    , generateFieldAccessors    = True
    , allFieldsStrict           = True
    }

-- | Default options for "Purescript style" records
--
-- That is:
--
-- * All fields are strict
-- * We do /not/ generate field accessors: fields must be accessed and updated
--   through the 'HasField' instances (e.g., @record-dot-preprocessor@ syntax).
--
-- We do not introduce a pattern synonym by default:
--
-- * Introducing a pattern synonym reintroduces code that is quadratic in size.
-- * Perhaps more importantly, it would make it impossible to define two records
--   with the same field names in a single module, as the field accessors
--   (unnecessarily but currently unavoidably) introduced by the pattern synonym
--   would clash.
--
-- NOTE: The @record-dot-preprocessor@ enables @DuplicateRecordFields@ by
-- default. Since the records that we produce are not visible to @ghc@,
-- @large-records@ is not compatible with DRF-style overloading. However, as
-- long as all overloading is resolved through @HasField@ instead (which is
-- what @record-dot-preprocessor@ encourages anyway), all is fine.
defaultPureScript :: Options
defaultPureScript = Options {
      generatePatternSynonym    = UseQuasiQuoter
    , generateConstructorFn     = True
    , generateHasFieldInstances = True
    , generateFieldAccessors    = False
    , allFieldsStrict           = True
    }
