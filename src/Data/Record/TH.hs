{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ViewPatterns        #-}

module Data.Record.TH (
    largeRecord
    -- * Options
  , Options(..)
  , defaultStrictOptions
  , defaultLazyOptions
  , defaultPureScript
    -- ** Helper functions for setting options
  , defaultConstructorFn
  , firstToLower
    -- ** Internal representation of a record
    --
    -- This is primarily exported for power users who need to override options
    -- in a specific way.
  ) where

import Control.Monad.Except (Except, runExcept, throwError)
import Data.Char (toLower)
import Data.Coerce (coerce)
import Data.Either (partitionEithers)
import Data.Generics (everything, mkQ)
import Data.List (intercalate)
import Data.Proxy
import Data.Vector (Vector)
import GHC.Exts (Any)
import GHC.Records.Compat (HasField(..))
import Unsafe.Coerce (unsafeCoerce)

import qualified Data.Kind   as Kind
import qualified Data.Vector as V

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Data.Record.Generic
import Data.Record.Generic.Eq
import Data.Record.Generic.Show

import qualified Data.Record.Generic.Rep as Rep

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
      -- in the record, and we're back to code that is quadratic in size.
      --
      -- Avoid if possible.
      generatePatternSynonym :: Bool

      -- | Generate a "constructor function" for the record
      --
      -- > mkT :: Word -> Bool -> Char -> a -> [b] -> T a b
      -- > mkT = ..
      --
      -- In the absence of a pattern synonym, this makes it possible to still
      -- be able to construct values of the record in a reasonably convenient
      -- way (albeit with positional rather than named arguments).
    , generateConstructorFn :: Record -> Maybe String

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

defaultLazyOptions :: Options
defaultLazyOptions = Options {
      generatePatternSynonym    = False
    , generateConstructorFn     = defaultConstructorFn
    , generateHasFieldInstances = True
    , generateFieldAccessors    = True
    , allFieldsStrict           = False
    }

defaultStrictOptions :: Options
defaultStrictOptions = Options {
      generatePatternSynonym    = False
    , generateConstructorFn     = defaultConstructorFn
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
      generatePatternSynonym    = False
    , generateConstructorFn     = defaultConstructorFn
    , generateHasFieldInstances = True
    , generateFieldAccessors    = False
    , allFieldsStrict           = True
    }

-- | Generate constructor function with the same name as the constructor,
-- but starting with a lower case letter.
defaultConstructorFn :: Record -> Maybe String
defaultConstructorFn = Just . firstToLower . recordConstr

firstToLower :: String -> String
firstToLower []     = []
firstToLower (c:cs) = toLower c : cs

{-------------------------------------------------------------------------------
  Public API
-------------------------------------------------------------------------------}

largeRecord :: Options -> Q [Dec] -> Q [Dec]
largeRecord opts decls = do
    (errs, rs) <- matchRecords <$> decls
    case errs of
      [] ->
        concatMapM (genAll opts) rs
      _otherwise ->
        fail $ intercalate "\n    " .concat $ [
            errs
          , ["(" ++ show (length errs) ++ " errors)"]
          ]
  where
    matchRecords :: [Dec] -> ([String], [Record])
    matchRecords = partitionEithers . map (runExcept . matchRecord)

-- | Generate all definitions
genAll :: Options -> Record -> Q [Dec]
genAll opts@Options{..} r = concatM $ [
      (:[]) <$> genNewtype opts r
    , genIndexedAccessor   opts r
    , genIndexedOverwrite  opts r
    , when generateHasFieldInstances $ [
          genHasFieldInstances opts r
        ]
    , case generateConstructorFn r of
        Just nm -> genConstructorFn opts r nm
        Nothing -> return []
      -- If we generate the pattern synonym, there is no need to generate
      -- field accessors, because GHC will generate them from the synonym
    , when (generateFieldAccessors && not generatePatternSynonym) $ [
          genFieldAccessors opts r
        ]
    , when generatePatternSynonym $ [
          genRecordView opts r
        , genPatSynonym opts r
        ]
    , genGenericInstance opts r
    ]
  where
    when :: Bool -> [Q [Dec]] -> Q [Dec]
    when False _   = return []
    when True  gen = concatM gen

{-------------------------------------------------------------------------------
  Generation: the type itself

  NOTE: All generation examples assume as example

  > data T a b = MkT {
  >       tWord  :: Word
  >     , tBool  :: Bool
  >     , tChar  :: Char
  >     , tA     :: a
  >     , tListB :: [b]
  >     }
  >   deriving (Eq, Show)
-------------------------------------------------------------------------------}

-- | Generate the newtype that will represent the record
--
-- Generates something like
--
-- > newtype T a b = TFromVector {vectorFromT :: Vector Any}
genNewtype :: Options -> Record -> Q Dec
genNewtype opts r@Record{..} =
    newtypeD
      (cxt [])
      nameType
      recordTVars
      Nothing
      (recC nameConstr [
           varBangType nameField $
             bangType (return DefaultBang) [t| Vector Any |]
         ])
      []
  where
    nameType, nameConstr, nameField :: Name
    nameType   = nameRecord               opts r
    nameConstr = nameRecordInternalConstr opts r
    nameField  = nameRecordInternalField  opts r

{-------------------------------------------------------------------------------
  Generation: field accessors
-------------------------------------------------------------------------------}

-- | Generate the indexed field accessor
--
-- Generates something like
--
-- > unsafeGetIndexT :: forall x a b. Int -> T a b -> x
-- > unsafeGetIndexT = \ n t -> unsafeCoerce (V.unsafeIndex (vectorFromT t) n)
genIndexedAccessor :: Options -> Record -> Q [Dec]
genIndexedAccessor opts r@Record{..} = do
    x <- newName "x"
    simpleFn
      (nameRecordIndexedAccessor opts r)
      (forallT
         (PlainTV x : recordTVars)
         (cxt [])
         (fnQ [conT ''Int, recordTypeQ opts r] (varT x)))
      [| \n t -> unsafeCoerce (V.unsafeIndex ($(recordToVectorQ opts r) t) n) |]

-- | Generate index field overwrite
--
-- Generates something like
--
-- > unsafeSetIndexT :: forall x a b. Int -> T a b -> x -> T a b
-- > unsafeSetIndexT = \n t val ->
-- >     TFromVector (V.unsafeUpd (vectorFromT t) [(n, unsafeCoerce val)])
--
-- If using 'allFieldsStrict', the function will be strict in @val@.
--
-- TODO: We should support per-field strictness.
genIndexedOverwrite :: Options -> Record -> Q [Dec]
genIndexedOverwrite opts@Options{..} r@Record{..} = do
    x <- newName "x"
    simpleFn
      (nameRecordIndexedOverwrite opts r)
      (forallT
        (PlainTV x : recordTVars)
        (cxt [])
        (fnQ [conT ''Int, recordTypeQ opts r, varT x] (recordTypeQ opts r)))
      body
  where
    body :: Q Exp
    body
      | allFieldsStrict =
          [| \n t !val -> $(recordFromVectorQ opts r) (
                 V.unsafeUpd ($(recordToVectorQ opts r) t) [(n, unsafeCoerce val)]
               )
           |]
      | otherwise =
          [| \n t val -> $(recordFromVectorQ opts r) (
                 V.unsafeUpd ($(recordToVectorQ opts r) t) [(n, unsafeCoerce val)]
               )
           |]

-- | Generate field accessors for all fields
genFieldAccessors :: Options -> Record -> Q [Dec]
genFieldAccessors opts r@Record{..} =
    concatMapM (genFieldAccessor opts r) recordFields

-- | Generate accessor for single field
--
-- Generates function such as
--
-- > tWord :: forall a b. T a b -> Word
-- > tWord = unsafeGetIndexT 0
genFieldAccessor :: Options -> Record -> Field -> Q [Dec]
genFieldAccessor opts r@Record{..} f = do
    simpleFn
      (nameFieldAccessor opts f)
      (forallT recordTVars (cxt []) $
         fnQ [recordTypeQ opts r] (fieldTypeQ opts f))
      (fieldUntypedAccessor opts r f)

-- | Generate 'HasField' instances for all fields
genHasFieldInstances :: Options -> Record -> Q [Dec]
genHasFieldInstances opts r@Record{..} =
    mapM (genHasFieldInstance opts r) recordFields

-- | Generate 'HasField' instance for single field
--
-- Generates something like
--
-- > instance HasField "tInt" (T a b) Word where
-- >   hasField = \t -> (unsafeSetIndexT 0 t, unsafeGetIndexT 0 t)
genHasFieldInstance :: Options -> Record -> Field -> Q Dec
genHasFieldInstance opts r f@Field{..} = do
    instanceD
      (cxt [])
      (appsT (conT ''HasField) [
          litT (strTyLit fieldUnqual)
        , recordTypeQ opts r
        , fieldTypeQ  opts f
        ])
      [valD (varP 'hasField) (normalB [|
          \t -> ( $(fieldUntypedOverwrite opts r f) t
                , $(fieldUntypedAccessor  opts r f) t
                )
        |]) []]

{-------------------------------------------------------------------------------
  Generation: pattern synonym
-------------------------------------------------------------------------------}

-- | Generate view on the record
--
-- Generates function such as
--
-- > tupleFromT :: forall a b. T a b -> (Word, Bool, Char, a, [b])
-- > tupleFromT = \x -> (
-- >       unsafeGetIndexT 0 x
-- >     , unsafeGetIndexT 1 x
-- >     , unsafeGetIndexT 2 x
-- >     , unsafeGetIndexT 3 x
-- >     , unsafeGetIndexT 4 x
-- >     )
--
-- Modulo tuple nesting (see 'nest').
genRecordView :: Options -> Record -> Q [Dec]
genRecordView opts r@Record{..} = do
    simpleFn
      (nameRecordView opts r)
      (forallT recordTVars (cxt []) $ fnQ [recordTypeQ opts r] viewType)
      viewBody
  where
    viewType :: Q Type
    viewType = mkTupleT (fieldTypeQ opts) $ nest recordFields

    viewBody :: Q Exp
    viewBody = do
        x <- newName "x"
        lamE [varP x] $ mkTupleE (viewField x) $ nest recordFields

    -- We generate the view only if we are generating the pattern synonym,
    -- but when we do we don't generate the typed accessors, because they
    -- are instead derived from the pattern synonym by GHC. Since the synonym
    -- requires the view, we therefore use the untyped accessor here.
    viewField :: Name -> Field -> Q Exp
    viewField x f = [| $(fieldUntypedAccessor opts r f) $(varE x) |]

-- | Construct a value of the record
--
-- Generates something like
--
-- > \tWord' tBool' tChar' tA' tListB' -> TFromVector (V.fromList [
-- >   , unsafeCoerce tWord'
-- >   , unsafeCoerce tBool'
-- >   , unsafeCoerce tChar'
-- >   , unsafeCoerce tA'
-- >   , unsafeCoerce tListB'
-- >   ])
--
-- However, this function is slightly more general than this, eneralizing over
-- the "kind of lambda" we want to construct. We use this both in
-- 'genPatSynonym' and in 'genConstructorFn'.
genRecordVal :: Options -> Record -> ([Q Pat] -> Q Exp -> Q a) -> Q a
genRecordVal opts r@Record{..} mkFn = do
    vars <- mapM mkVarName recordFields
    mkFn (map varP vars)
         [| $(recordFromVectorQ opts r) $(mkVector qUnsafeCoerce vars) |]
  where
    qUnsafeCoerce :: Name -> Q Exp
    qUnsafeCoerce x = [| unsafeCoerce $(varE x) |]

    -- The constructor arguments are locally bound, and should not have the
    -- same name as the fields themselves
    mkVarName :: Field -> Q Name
    mkVarName = newName . fieldUnqual

-- | Generate pattern synonym
--
-- Constructs something like this:
--
-- > pattern MkT :: forall a b. Word -> Bool -> Char -> a -> [b] -> T a b
-- > pattern MkT{tWord, tBool, tChar, tA, tListB} <-
-- >     (tupleFromT -> (tWord, tBool, tChar, tA, tListB) )
-- >   where
-- >     MkT tWord' tBool' tChar' tA' tListB' = ..
-- >
-- > {-# COMPLETE MkT #-}
--
-- modulo nesting ('nest'), where the body of 'MkT' (and its arguments) are
-- constructed by 'genRecordVal'.
genPatSynonym :: Options -> Record -> Q [Dec]
genPatSynonym opts r@Record{..} = sequence [
      patSynSigD (mkName recordConstr) $
        simplePatSynType
          recordTVars
          (map (fieldTypeQ opts) recordFields)
          (recordTypeQ opts r)
    , patSynD (mkName recordConstr)
        (recordPatSyn $ map (nameFieldAccessor opts) recordFields)
        qDir
        matchVector
    , pragCompleteD [mkName recordConstr] Nothing
    ]
  where
    matchVector :: Q Pat
    matchVector = viewP (varE (nameRecordView opts r)) $
        mkTupleP (varP . nameFieldAccessor opts) $ nest recordFields

    constrVector :: [Q Pat] -> Q Exp -> Q Clause
    constrVector xs body = clause xs (normalB body) []

    qDir :: Q PatSynDir
    qDir = explBidir . (:[]) $ genRecordVal opts r constrVector

{-------------------------------------------------------------------------------
  Generation: constructor function
-------------------------------------------------------------------------------}

-- | Generate constructor function
--
-- Generates something like
--
-- > mkT :: forall a b. Word -> Bool -> Char -> a -> [b] -> T a b
-- > mkT = ..
--
-- where the body of @mkT@ is generated by 'genRecordVal'.
genConstructorFn ::
     Options
  -> Record
  -> String  -- ^ Name of the constructor function
  -> Q [Dec]
genConstructorFn opts r@Record{..} nm = do
    simpleFn
      (mkName nm)
      (forallT recordTVars (cxt []) $
         fnQ (map (fieldTypeQ opts) recordFields) (recordTypeQ opts r))
      (genRecordVal opts r lamE)

{-------------------------------------------------------------------------------
  Generation: Generic instance
-------------------------------------------------------------------------------}

-- | Generate the class we will use to instantiate 'Constraints'
--
-- Generates something like this:
--
-- > class Constraints_T a b (c :: Type -> Constraint) where
-- >   dictConstraints_T :: Proxy c -> Rep (Dict c) (T a b)
--
-- NOTE: It is critical that we don't give the class any superclass constraints
-- like
--
-- > class (c Word, c Bool, c Char, c a, c [b])
-- >    => Constraints_T a b (c :: Type -> Constraint)
--
-- because then @ghc@ would use resolve @Constraints_T@ to that tuple instead,
-- and use lots of "tuple constraint extractor" functions, each of which have
-- the same size as the number of constraints (another example of a
-- @case f of { T x1 x2 x3 .. -> xn@ function, but now at the dictionary level).
genConstraintsClass :: Options -> Record -> Q Dec
genConstraintsClass opts r = do
    c <- newName "c"
    k <- [t| Kind.Type -> Kind.Constraint |]
    classD
      (cxt [])
      (nameRecordConstraintsClass opts r)
      (recordTVars r ++ [KindedTV c k])
      []
      [ sigD (nameRecordConstraintsMethod opts r)
             [t| Proxy $(varT c) -> Rep (Dict $(varT c)) $(recordTypeQ opts r) |]
      ]

-- | Superclass constraints required by the constraints class instance
--
-- Generates something like
--
-- > (c Word, c Bool, c Char, c a, c [b])
--
-- However, we filter out constraints that are type variable free, so if we
-- pass, say, @Show@ for @c@, then we generate
--
-- > (Show a, Show [b])
--
-- instead. This avoids @ghc@ complaining about
--
-- > Redundant constraints: (Show Word, Show Bool, Show Char)
genRequiredConstraints :: Options -> Record -> Q Type -> Q Cxt
genRequiredConstraints opts Record{..} c = do
    constraints <- mapM constrainField recordFields
    return $ filter hasTypeVar constraints
  where
    constrainField :: Field -> Q Pred
    constrainField f = c `appT` fieldTypeQ opts f

    hasTypeVar :: Pred -> Bool
    hasTypeVar = everything (||) (mkQ False isTypeVar)

    isTypeVar :: Type -> Bool
    isTypeVar (VarT _)   = True
    isTypeVar _otherwise = False

-- | Generate the dictionary creation function ('dict')
--
-- Generates something like
--
-- > \p -> Rep (V.fromList [
-- >     unsafeCoerce (dictFor p (Proxy :: Proxy Word))
-- >   , unsafeCoerce (dictFor p (Proxy :: Proxy Bool))
-- >   , unsafeCoerce (dictFor p (Proxy :: Proxy Char))
-- >   , unsafeCoerce (dictFor p (Proxy :: Proxy a))
-- >   , unsafeCoerce (dictFor p (Proxy :: Proxy [b]))
-- >   ])
genDict :: Options -> Record -> Q Exp
genDict opts Record{..} = do
    p <- newName "p"
    lamE [varP p] [| Rep $(mkVector (dictForField p) recordFields) |]
  where
    dictForField :: Name -> Field -> Q Exp
    dictForField p f = [|
          unsafeCoerce (dictFor $(varE p) (Proxy :: Proxy $(fieldTypeQ opts f)))
        |]

-- | Generate (one and only) instance of the constraints class
--
-- Generates something like
--
-- > instance (..) => Constraints_T a b c where
-- >   dictConstraints_T = ..
--
-- where the body of @dictConstraints_T@ is generated by 'genDict'.
genConstraintsClassInstance :: Options -> Record -> Q Dec
genConstraintsClassInstance opts r@Record{..} = do
    c <- newName "c"
    instanceD
      (genRequiredConstraints opts r (varT c))
      (appsT (conT (nameRecordConstraintsClass opts r)) $
         map tyVarType recordTVars ++ [varT c])
      [ valD (varP (nameRecordConstraintsMethod opts r))
             (normalB (genDict opts r))
             []
      ]

-- | Generate the Constraints type family instance
--
-- Generates something like
--
-- > type Constraints (T a b) = Constraints_T a b
genConstraintsFamilyInstance :: Options -> Record -> Q Dec
genConstraintsFamilyInstance opts r@Record{..} = tySynInstD $
    tySynEqn
      Nothing
      [t| Constraints $(recordTypeQ opts r) |]
      (appsT (conT (nameRecordConstraintsClass opts r)) $
         map tyVarType recordTVars)

-- | Generate metadata
--
-- Generates something like
--
-- > \_p -> Metadata {
-- >     recordName        = "T",
-- >   , recordConstructor = "MkT"
-- >   , recordFieldNames  = unsafeFromListK ["tWord", "tBool", "tChar", "tA", "tListB"]
-- >   }
genMetadata :: Options -> Record -> Q Exp
genMetadata _opts Record{..} = do
    p <- newName "_p"
    lamE [varP p] $ recConE 'Metadata [
        fieldExp 'recordName        $ litE (stringL recordUnqual)
      , fieldExp 'recordConstructor $ litE (stringL recordConstr)
      , fieldExp 'recordSize        $ litE (integerL numFields)
      , fieldExp 'recordFieldNames  $ [| Rep.unsafeFromListK $fieldNames |]
      ]
  where
    numFields :: Integer
    numFields = fromIntegral $ length recordFields

    fieldNames :: Q Exp
    fieldNames = listE $ map (litE . stringL . fieldUnqual) recordFields

-- | Generate instance for specific class
--
-- Generates one of the following:
--
-- * 'Show':
--
--   > instance (..) => Eq (T a b) where
--   >   (==) = geq
--
-- * 'Eq':
--
--   > instance (..) => Show (T a b) where
--   >   showsPrec = gshowsPrec
--
-- where the @(..)@ constraints are generated by 'genRequiredConstraints'
-- (i.e., a constraint for each field).
--
-- TODO: Think about DeriveFunctor?
genDeriving :: Options -> Record -> Deriving -> Q Dec
genDeriving opts r@Record{..} = \case
    DeriveEq   -> inst ''Eq   '(==)      'geq
    DeriveShow -> inst ''Show 'showsPrec 'gshowsPrec
  where
    inst :: Name -> Name -> Name -> Q Dec
    inst clss fn gfn =
        instanceD
          (genRequiredConstraints opts r (conT clss))
          [t| $(conT clss) $(recordTypeQ opts r) |]
          [valD (varP fn) (normalB (varE gfn)) []]

-- | Generate definition for `from` in the `Generic` instance
--
-- Generates something like
--
-- > repFromVectorStrict . vectorFromT
genFrom :: Options -> Record -> Q Exp
genFrom opts r = [| repFromVector . $(varE (nameRecordInternalField opts r)) |]

-- | Generate definition for `to` in the `Generic` instance
--
-- Generation depends on options.
--
-- * If all fields are strict, generates something like
--
--   > TFromVector . repToVector
--
-- * Otherwise, generates something like
--
--   > TFromVector . repToVector
--
-- TODO: We should support per-field strictness annotations.
genTo :: Options -> Record -> Q Exp
genTo opts@Options{..} r
  | allFieldsStrict = [| $(conE (nameRecordInternalConstr opts r)) . repToVectorStrict |]
  | otherwise       = [| $(conE (nameRecordInternalConstr opts r)) . repToVectorLazy   |]

-- | Generate the definitions required to provide the instance for 'Generic'
--
-- > instance Generic T where
-- >   type Constraints T = Constraints_T
-- >   from       = coerce
-- >   to         = coerce
-- >   dict       = dictConstraints_T
-- >   metadata   = ..
genGenericInstance :: Options -> Record -> Q [Dec]
genGenericInstance opts r@Record{..} = concatM [
       sequence [
           genConstraintsClass         opts r
         , genConstraintsClassInstance opts r
         , instanceD
             (cxt [])
             [t| Generic $(recordTypeQ opts r) |]
             [ genConstraintsFamilyInstance opts r
              , valD (varP 'from)     (normalB (genFrom opts r))                            []
              , valD (varP 'to)       (normalB (genTo   opts r))                            []
              , valD (varP 'dict)     (normalB (varE (nameRecordConstraintsMethod opts r))) []
              , valD (varP 'metadata) (normalB (genMetadata opts r))                        []
             ]
         ]
    , mapM (genDeriving opts r) recordDeriv
    ]

{-------------------------------------------------------------------------------
  Decide naming
-------------------------------------------------------------------------------}

nameRecord :: Options -> Record -> Name
nameRecord _opts Record{..} =
    mkName $ recordUnqual

-- | The name of the constructor used internally
--
-- For example, if the user defines
--
-- > [largeRecord| data T = MkT {..} |]
--
-- then
--
-- * 'recordUnqual'             will be 'T'
-- * 'recordConstr'             will be 'MkT'
-- * 'nameRecordInternalConstr' will be 'TFromVector'
-- * 'nameRecordInternalField'  will be 'vectorFromT'
nameRecordInternalConstr :: Options -> Record -> Name
nameRecordInternalConstr _opts Record{..} =
     mkName $ recordUnqual ++ "FromVector"

-- | The name of the newtype unwrapper used internally
--
-- See 'nameRecordInternalConstr' for a detailed discussion.
nameRecordInternalField :: Options -> Record -> Name
nameRecordInternalField _opts Record{..} =
    mkName $ "vectorFrom" ++ recordUnqual

nameRecordView :: Options -> Record -> Name
nameRecordView _opts Record{..} =
    mkName $ "tupleFrom" ++ recordUnqual

nameRecordIndexedAccessor :: Options -> Record -> Name
nameRecordIndexedAccessor _opts Record{..} =
    mkName $ "unsafeGetIndex" ++ recordUnqual

nameRecordIndexedOverwrite :: Options -> Record -> Name
nameRecordIndexedOverwrite _opts Record{..} =
    mkName $ "unsafeSetIndex" ++ recordUnqual

nameRecordConstraintsClass :: Options -> Record -> Name
nameRecordConstraintsClass _opts Record{..} =
    mkName $ "Constraints_" ++ recordUnqual

nameRecordConstraintsMethod :: Options -> Record -> Name
nameRecordConstraintsMethod _opts Record{..} =
    mkName $ "dictConstraints_" ++ recordUnqual

nameFieldAccessor :: Options -> Field -> Name
nameFieldAccessor _opts Field{..} =
    mkName $ fieldUnqual

{-------------------------------------------------------------------------------
  Derived
-------------------------------------------------------------------------------}

recordTypeQ :: Options -> Record -> Q Type
recordTypeQ opts r@Record{..} =
    appsT (conT (nameRecord opts r)) $ map tyVarType recordTVars

recordToVectorQ :: Options -> Record -> Q Exp
recordToVectorQ opts = varE . nameRecordInternalField opts

recordFromVectorQ :: Options -> Record -> Q Exp
recordFromVectorQ opts = conE . nameRecordInternalConstr opts

recordIndexedAccessorQ :: Options -> Record -> Q Exp
recordIndexedAccessorQ opts = varE . nameRecordIndexedAccessor opts

recordIndexedOverwriteQ :: Options -> Record -> Q Exp
recordIndexedOverwriteQ opts = varE . nameRecordIndexedOverwrite opts

fieldTypeQ :: Options -> Field -> Q Type
fieldTypeQ _opts Field{..} = return fieldType

fieldIndexQ :: Options -> Field -> Q Exp
fieldIndexQ _opts Field{..} = litE . integerL $ fromIntegral fieldIndex

fieldUntypedAccessor :: Options -> Record -> Field -> Q Exp
fieldUntypedAccessor opts r f =
    [| $(recordIndexedAccessorQ opts r) $(fieldIndexQ opts f) |]

fieldUntypedOverwrite :: Options -> Record -> Field -> Q Exp
fieldUntypedOverwrite opts r f =
    [| $(recordIndexedOverwriteQ opts r) $(fieldIndexQ opts f) |]

{-------------------------------------------------------------------------------
  Supported declarations
-------------------------------------------------------------------------------}

-- | Unqualified name
type Unqual = String

-- | Our internal representation of a record
--
-- We parse the 'Dec' that TH gives us and represent it in a more convenient way
-- for processing. For our running example, the internal representation looks
-- like this:
--
-- > Record {
-- >     recordUnqual = "T"
-- >   , recordConstr = "MkT"
-- >   , recordFields = [
-- >         Field {fieldUnqual = "tInt"   , fieldType = ConT 'Word          , fieldIndex = 0}
-- >       , Field {fieldUnqual = "tBool"  , fieldType = ConT 'Bool          , fieldIndex = 1}
-- >       , Field {fieldUnqual = "tChar"  , fieldType = ConT 'Char          , fieldIndex = 2}
-- >       , Field {fieldUnqual = "tA"     , fieldType = VarT a              , fieldIndex = 3}
-- >       , Field {fieldUnqual = "tListB" , fieldType = AppT ListT (VarT b) , fieldIndex = 4}
-- >       ]
-- >   , recordDeriv = [DeriveEq, DeriveShow]
-- >   , recordTVars = [PlainTV a, PlainTV b]
-- >   }
data Record = Record {
      -- | Unqualified name of the record type
      recordUnqual :: Unqual

      -- | The type variables in the record type
    , recordTVars :: [TyVarBndr]

      -- | Unqualified name of the record constructor
    , recordConstr :: Unqual

      -- | The fields in the record
    , recordFields :: [Field]

      -- | The type class instances that should be derived
    , recordDeriv :: [Deriving]
    }
  deriving (Show)

data Field = Field {
      -- | Unqualified name of the field
      fieldUnqual :: Unqual

      -- | Type of the field
    , fieldType :: Type

      -- | Index of the field (field 0, field 1, ..)
      --
      -- This is strictly speaking redundant information, as this is already
      -- implied by the position of the field in 'recordFields'. However, since
      -- we do a lot of positional processing (every field corresponds to a
      -- vector element), it is convenient to have the index readily available.
    , fieldIndex  :: Int
    }
  deriving (Show)

data Deriving =
    DeriveEq
  | DeriveShow
  deriving (Show)

matchRecord :: Dec -> Except String Record
matchRecord (DataD
          _cxt@[]
          name
          tyVarBndrs
          _kind@Nothing
          [RecC constrName fields]
          derivClauses
       ) =
        Record (nameBase name) tyVarBndrs (nameBase constrName)
    <$> mapM matchField (zip [0..] fields)
    <*> concatMapM matchDeriv derivClauses
matchRecord d =
    throwError $ "Unsupported declaration: " ++ show d

-- | Support deriving clauses
--
-- TODO: We'll want to support some additional built-in classes probably,
-- and we can for sure support 'DeriveAnyClass' style derivation.
matchDeriv :: DerivClause -> Except String [Deriving]
matchDeriv (DerivClause Nothing cs) = mapM go cs
  where
    go :: Pred -> Except String Deriving
    go p | p == ConT ''Eq   = return DeriveEq
         | p == ConT ''Show = return DeriveShow
         | otherwise = throwError $ "Cannot derive instance for " ++ show p
matchDeriv (DerivClause (Just _) _) =
    throwError "Deriving strategies not supported"

matchField :: (Int, VarBangType)-> Except String Field
matchField (i, (name, bng, typ)) =
    case bng of
      DefaultBang -> return $ Field (unqualify name) typ i
      _otherwise  -> throwError $ "Unsupported bang type: " ++ show bng
  where
    unqualify :: Name -> Unqual
    unqualify = undoDRF . nameBase

-- When @DuplicateRecordFields@ is enabled, it produces field names such as
-- @$sel:a:MkY@. We don't really care much about 'DuplicateRecordFields',
-- insofar as that we will not try to be compatible with DRF-style
-- overloading (all overloading must happen through 'HasField' instead).
-- We do however need to recover the original field name.
--
-- <https://gitlab.haskell.org/ghc/ghc/-/wikis/records/overloaded-record-fields/duplicate-record-fields>
-- <https://gitlab.haskell.org/ghc/ghc/-/issues/14848>
undoDRF :: String -> String
undoDRF name =
   case name of
     '$' : drf  -> takeWhile (/= ':') . tail . dropWhile (/= ':') $ drf
     _otherwise -> name

pattern DefaultBang :: Bang
pattern DefaultBang = Bang NoSourceUnpackedness NoSourceStrictness

{-------------------------------------------------------------------------------
  Auxiliary: working with tuples
-------------------------------------------------------------------------------}

-- | Construct tuple type
mkTupleT :: forall a. (a -> Q Type) -> Forest a -> Q Type
mkTupleT f = forest cata
  where
    cata :: Cata a (Q Type)
    cata = Cata {
          leaf   = f
        , branch = \case [t] -> t
                         ts  -> appsT (tupleT (length ts)) ts
        }

-- | Construct tuple expression
mkTupleE :: forall a. (a -> Q Exp) -> Forest a -> Q Exp
mkTupleE f = forest cata
  where
     cata :: Cata a (Q Exp)
     cata = Cata {
           leaf   = f
         , branch = \case [e] -> e
                          es  -> tupE es
         }

-- | Construct tuple pattern
mkTupleP :: forall a. (a -> Q Pat) -> Forest a -> Q Pat
mkTupleP f = forest cata
  where
    cata :: Cata a (Q Pat)
    cata = Cata {
          leaf   = f
        , branch = \case [p] -> p
                         ps  -> tupP ps
        }

-- | Observe Haskell's tuple length
--
-- Haskell has a limit of 62 fields per tuple. Here we take an arbitrary
-- list and turn it into a nested tuple that preserves this limit.
--
-- Example: if we reduce the limit to @2@, we get the following nestings,
-- for lengths @[1..10]@:
--
-- >     A
-- >    (A, A)
-- >   ((A, A),  A)
-- >   ((A, A), (A, A))
-- >  (((A, A), (A, A)),   A)
-- >  (((A, A), (A, A)),  (A, A))
-- >  (((A, A), (A, A)), ((A, A),  A))
-- >  (((A, A), (A, A)), ((A, A), (A, A)))
-- > ((((A, A), (A, A)), ((A, A), (A, A))),  A)
-- > ((((A, A), (A, A)), ((A, A), (A, A))), (A, A))
nest :: forall a. [a] -> Forest a
nest = go . map Leaf
  where
    go :: [Tree a] -> Forest a
    go ts | length ts < limit = Forest ts
          | otherwise         = go (map (Branch . Forest) (chunk limit ts))

    limit :: Int
    limit = 62

{-------------------------------------------------------------------------------
  Auxiliary: trees and large bananas
-------------------------------------------------------------------------------}

-- | Trees with values at the leaves
data Tree   a = Leaf a | Branch (Forest a) deriving (Show)
data Forest a = Forest [Tree a]            deriving (Show)

-- | Catamorphism over trees/forests
--
-- Unlike regular folds, these catamorphisms are structure preserving.
-- See "Dealing with Large Bananas", by Ralf Lämmel et al
data Cata a b = Cata {
      leaf   :: a -> b
    , branch :: [b] -> b
    }

tree :: Cata a b -> Tree a -> b
tree cata (Leaf   a)  = leaf   cata a
tree cata (Branch as) = forest cata as

forest :: Cata a b -> Forest a -> b
forest cata (Forest ts) = branch cata (map (tree cata) ts)

{-------------------------------------------------------------------------------
  Auxiliary: TH
-------------------------------------------------------------------------------}

simpleFn :: Name -> Q Type -> Q Exp -> Q [Dec]
simpleFn name qTyp qBody = do
    typ  <- qTyp
    body <- qBody
    return [
          SigD name typ
        , ValD (VarP name) (NormalB body) []
        ]

-- | Construct simple pattern synonym type
simplePatSynType :: [TyVarBndr] -> [Q Type] -> Q Type -> Q PatSynType
simplePatSynType tvars fieldTypes resultType =
      forallT tvars (cxt [])
    $ forallT []    (cxt [])
    $ fnQ fieldTypes resultType

mkVector :: (a -> Q Exp) -> [a] -> Q Exp
mkVector f elems = [| V.fromList $(listE (map f elems)) |]

fnQ :: [Q Type] -> Q Type -> Q Type
fnQ ts t = foldr (\a b -> arrowT `appT` a `appT` b) t ts

appsT :: Q Type -> [Q Type] -> Q Type
appsT t ts = foldl appT t ts

tyVarName :: TyVarBndr -> Name
tyVarName (PlainTV  n)   = n
tyVarName (KindedTV n _) = n

tyVarType :: TyVarBndr -> Q Type
tyVarType = varT . tyVarName

{-------------------------------------------------------------------------------
  Auxiliary: general
-------------------------------------------------------------------------------}

concatM :: Applicative m => [m [a]] -> m [a]
concatM = fmap concat . sequenceA

concatMapM :: Applicative m => (a -> m [b]) -> [a] -> m [b]
concatMapM f = concatM . map f

chunk :: Int -> [a] -> [[a]]
chunk n = go
  where
    go :: [a] -> [[a]]
    go [] = []
    go xs = let (firstChunk, rest) = splitAt n xs in firstChunk : go rest

{-------------------------------------------------------------------------------
  Functions to support the TH code (i.e., functions called by generated code)

  NOTE: We leave the generic representation type as lazy, and only force
  values once we translate back to the type itself. This means that we can
  chain generic functions and get some kind of fusion without having to
  traverse records multiple times.
-------------------------------------------------------------------------------}

dictFor :: c x => Proxy c -> Proxy x -> Dict c x
dictFor _ _ = Dict

repFromVector :: Vector Any -> Rep I a
repFromVector = coerce

repToVectorLazy :: Rep I a -> Vector Any
repToVectorLazy = coerce

repToVectorStrict :: Rep I a -> Vector Any
repToVectorStrict (Rep v) =
    rnfElems (V.toList v) `seq` coerce v
  where
    rnfElems :: [I Any] -> ()
    rnfElems []       = ()
    rnfElems (I x:xs) = x `seq` rnfElems xs
