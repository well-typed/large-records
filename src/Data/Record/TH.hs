{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE ViewPatterns               #-}

module Data.Record.TH (
    largeRecord
  , endOfBindingGroup
  , mkRecord
    -- * Options
  , Options(..)
  , defaultStrictOptions
  , defaultLazyOptions
  , defaultPureScript
  ) where

import Data.Char (toLower)
import Data.Coerce (Coercible, coerce)
import Data.Generics (everything, mkQ)
import Data.Maybe (catMaybes)
import Data.Map (Map)
import Data.Proxy
import Data.Vector (Vector)
import GHC.Exts (Any)
import GHC.Records.Compat (HasField(..))
import Unsafe.Coerce (unsafeCoerce)

import qualified Data.Kind   as Kind
import qualified Data.Vector as V
import qualified Data.Map    as Map

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote

import qualified Language.Haskell.Meta.Parse as HSE

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
      -- The name of the constructor function is the name of the constructor,
      -- but starting with lowercase letter. This function can be used directly,
      -- but it is also used by 'constructRecord', so if this function is
      -- not generated, 'constructRecord' will not work.
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

defaultLazyOptions :: Options
defaultLazyOptions = Options {
      generatePatternSynonym    = False
    , generateConstructorFn     = True
    , generateHasFieldInstances = True
    , generateFieldAccessors    = True
    , allFieldsStrict           = False
    }

defaultStrictOptions :: Options
defaultStrictOptions = Options {
      generatePatternSynonym    = False
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
      generatePatternSynonym    = False
    , generateConstructorFn     = True
    , generateHasFieldInstances = True
    , generateFieldAccessors    = False
    , allFieldsStrict           = True
    }

{-------------------------------------------------------------------------------
  Record definition
-------------------------------------------------------------------------------}

largeRecord :: Options -> Q [Dec] -> Q [Dec]
largeRecord opts decls = do
    rs <- mapM matchRecord =<< decls
    concatMapM (genAll opts) (catMaybes rs)

-- | Generate all definitions
genAll :: Options -> Record -> Q [Dec]
genAll opts@Options{..} r = concatM $ [
      (:[]) <$> genNewtype opts r
    , genIndexedAccessor   opts r
    , genIndexedOverwrite  opts r
    , when generateHasFieldInstances $ [
          genHasFieldInstances opts r
        ]
    , when generateConstructorFn [
          genConstructorFn opts r
        ]
      -- If we generate the pattern synonym, there is no need to generate
      -- field accessors, because GHC will generate them from the synonym
    , when (generateFieldAccessors && not generatePatternSynonym) $ [
          genFieldAccessors opts r
        ]
    , when generatePatternSynonym $ [
          genRecordView opts r
        , genPatSynonym opts r
        ]
    , genTypeLevelMetadata opts r
    , genGenericInstance   opts r
    ]
  where
    when :: Bool -> [Q [Dec]] -> Q [Dec]
    when False _   = return []
    when True  gen = concatM gen

{-------------------------------------------------------------------------------
  Construct record values
-------------------------------------------------------------------------------}

mkRecord :: QuasiQuoter
mkRecord = QuasiQuoter {
      quoteExp  = go
    , quotePat  = wrongContext
    , quoteType = wrongContext
    , quoteDec  = wrongContext
    }
  where
    wrongContext :: String -> Q a
    wrongContext _ =  fail "mkRecord can only be used in expression contexts"

    go :: String -> Q Exp
    go str =
        case HSE.parseExp str of
          Left  err  -> fail $ "Could not parse record definition: " ++ err
          Right expr -> constructRecord expr

endOfBindingGroup :: Q [Dec]
endOfBindingGroup = return []

constructRecord :: Exp -> Q Exp
constructRecord expr = do
    RecordValue{..} <- matchRecordValue expr
    (_tyVars, mdata) <- getTypeLevelMetadata recordValueConstr
    aligned <- align (map fst mdata) recordValueFields
    appsE $ varE (nameConstructorFn recordValueConstr) : map return aligned

-- | Order record definition declarations according to the type declaration
--
-- Once we have the right order, we can then pass those arguments to the
-- "constructor function" we define in 'genConstructorFn'. Since that function
-- is typed, any type mismatches will then be handled by @ghc@.
align :: [FieldName] -> Map FieldName Exp -> Q [Exp]
align [] defs =
    if Map.null defs
      then return []
      else fail $ "Unexpected fields: " ++ show (Map.keys defs)
align (f:fs) defs =
    case Map.lookup f defs of
      Just t ->
        (t:) <$> align fs (Map.delete f defs)
      Nothing -> do
        reportWarning $ "Missing field " ++ show f
        (VarE 'undefined :) <$> align fs defs

{-------------------------------------------------------------------------------
  Generation: the type itself

  NOTE: All generation exampleshask assume as example

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
      (name recordUnqual)
      recordTVars
      Nothing
      (recC (nameRecordInternalConstr opts r) [
           varBangType (nameRecordInternalField opts r) $
             bangType (return DefaultBang) [t| Vector Any |]
         ])
      []

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
genFieldAccessor opts r@Record{..} f@Field{..} = do
    simpleFn
      (name fieldUnqual)
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
          nameType fieldUnqual
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
    -- The constructor arguments are locally bound, and should not have the
    -- same name as the fields themselves
    vars <- mapM (fresh . fieldUnqual) recordFields
    mkFn (map varP vars)
         [| $(recordFromVectorQ opts r) $(mkVector qUnsafeCoerce vars) |]
  where
    qUnsafeCoerce :: Name -> Q Exp
    qUnsafeCoerce x = [| unsafeCoerce $(varE x) |]

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
      patSynSigD (name recordConstr) $
        simplePatSynType
          recordTVars
          (map (fieldTypeQ opts) recordFields)
          (recordTypeQ opts r)
    , patSynD (name recordConstr)
        (recordPatSyn $ map (name . fieldUnqual) recordFields)
        qDir
        matchVector
    , pragCompleteD [name recordConstr] Nothing
    ]
  where
    matchVector :: Q Pat
    matchVector = viewP (varE (nameRecordView opts r)) $
        mkTupleP (varP . name . fieldUnqual) $ nest recordFields

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
genConstructorFn :: Options -> Record -> Q [Dec]
genConstructorFn opts r@Record{..} = do
    simpleFn
      (nameConstructorFn recordConstr)
      (forallT recordTVars (cxt []) $
         fnQ (map (fieldTypeQ opts) recordFields) (recordTypeQ opts r))
      (genRecordVal opts r lamE)

{-------------------------------------------------------------------------------
  Generation: type-level metadata
-------------------------------------------------------------------------------}

-- | Generate type-level metadata
--
-- Generates something like
--
-- > type Fields_T a b = '[
-- >     '("tInt"   , Word)
-- >   , '("tBool"  , Bool)
-- >   , '("tChar"  , Char)
-- >   , '("tA"     , a)
-- >   , '("tListB" , [b])
-- >   ]
--
-- TODO: We currently use this only for record construction, and don't tie it
-- in with the generics. I wonder if we could and/or should.
genTypeLevelMetadata :: Options -> Record -> Q [Dec]
genTypeLevelMetadata opts Record{..} = (:[]) <$>
    tySynD
      (nameRecordTypeLevelMetadata recordConstr)
      recordTVars
      (typeLevelList $ map fieldMetadata recordFields)
  where
    fieldMetadata :: Field -> Q Type
    fieldMetadata f@Field{..} = typeLevelTuple [
          nameType fieldUnqual
        , fieldTypeQ opts f
        ]

-- | Parse previously constructed type level data
--
-- We do this when we construct record /values/, at which point we have no
-- 'Options', so this must work without options.
getTypeLevelMetadata :: ConstrName -> Q ([TyVarBndr], [(FieldName, Type)])
getTypeLevelMetadata constr =
    parse =<< reify (nameRecordTypeLevelMetadata constr)
  where
    parse :: Info -> Q ([TyVarBndr], [(FieldName, Type)])
    parse (TyConI (TySynD _name bndrs typ)) = (bndrs,) <$> parseList typ
    parse i = fail $ concat [
          "Unrecognized metadata: "
        , show i
        , " (expected type synonym)"
        ]

    parseList :: Type -> Q [(FieldName, Type)]
    parseList (AppT (AppT PromotedConsT t) ts) =
        (:) <$> parseTuple t <*> parseList ts
    parseList PromotedNilT =
        return []
    parseList (SigT t _kind) =
        parseList t
    parseList t = fail $ concat [
          "Unexpected type: "
        , show t
        , " (expected type level list)"
        ]

    parseTuple :: Type -> Q (FieldName, Type)
    parseTuple (AppT (AppT (PromotedTupleT 2) (LitT (StrTyLit f))) t) =
        return (FieldName f, t)
    parseTuple t = fail $ concat [
          "Unexpected type: "
        , show t
        , " (expected type level tuple of a name and a type)"
        ]

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
        fieldExp 'recordName        $ nameExpr recordUnqual
      , fieldExp 'recordConstructor $ nameExpr recordConstr
      , fieldExp 'recordSize        $ litE (integerL numFields)
      , fieldExp 'recordFieldNames  $ [| Rep.unsafeFromListK $fieldNames |]
      ]
  where
    numFields :: Integer
    numFields = fromIntegral $ length recordFields

    fieldNames :: Q Exp
    fieldNames = listE $ map (nameExpr . fieldUnqual) recordFields

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

-- | The name of the constructor used internally
--
-- We pick this depending on whether the user enabled the generation of the
-- pattern synonym:
--
-- * If we generate the pattern synonym, then that pattern  synonym should have
--   the name of the original constructor, and the name we pick here must be
--   different.
--
-- * If however we do /not/ generate the pattern synonym, we pick the /original/
--   name here. We do this so that the constructor name is in scope, enabling
--   the user to write TH splices such as
--
--   > $(constructRecord [| MkR { x = 5, y = True } |])
--
--   For some reason (not sure why), this is not possible unless 'MkR' exists
--   (it doesn't need to have the right type, it just needs to exist).
nameRecordInternalConstr :: Options -> Record -> Name
nameRecordInternalConstr Options{..} Record{..}
  | generatePatternSynonym = nameWithSuffix "FromVector" $ recordUnqual
  | otherwise              = name                        $ recordConstr

-- | Name of the type synonym with the type-level metadata
--
-- We use the name of the /constructor/ for the type level metadata,
-- so that it's easier to find this info when we construct record values.
--
-- See 'constructRecord'.
nameRecordTypeLevelMetadata :: ConstrName -> Name
nameRecordTypeLevelMetadata = nameWithPrefix "Fields_"

-- | Name of the constructor function
--
-- NOTE: The name of the constructor function cannot depend on the options,
-- because it is needed by 'constructRecord'.
nameConstructorFn :: ConstrName -> Name
nameConstructorFn (ConstrName n) = mkName $ firstToLower n

nameRecordConstraintsClass  :: Options -> Record -> Name
nameRecordConstraintsMethod :: Options -> Record -> Name
nameRecordIndexedAccessor   :: Options -> Record -> Name
nameRecordIndexedOverwrite  :: Options -> Record -> Name
nameRecordInternalField     :: Options -> Record -> Name
nameRecordView              :: Options -> Record -> Name

nameRecordConstraintsClass  _opts = nameWithPrefix "Constraints_"     . recordUnqual
nameRecordConstraintsMethod _opts = nameWithPrefix "dictConstraints_" . recordUnqual
nameRecordIndexedAccessor   _opts = nameWithPrefix "unsafeGetIndex"   . recordUnqual
nameRecordIndexedOverwrite  _opts = nameWithPrefix "unsafeSetIndex"   . recordUnqual
nameRecordInternalField     _opts = nameWithPrefix "vectorFrom"       . recordUnqual
nameRecordView              _opts = nameWithPrefix "tupleFrom"        . recordUnqual

{-------------------------------------------------------------------------------
  Derived
-------------------------------------------------------------------------------}

recordTypeQ :: Options -> Record -> Q Type
recordTypeQ _opts Record{..} =
    appsT (conT (name recordUnqual)) $ map tyVarType recordTVars

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
  Names
-------------------------------------------------------------------------------}

class Coercible n String => IsName n where

newtype TypeName = TypeName String
  deriving newtype  (Show, Eq, Ord)
  deriving anyclass IsName

newtype ConstrName = ConstrName String
  deriving newtype  (Show, Eq, Ord)
  deriving anyclass IsName

newtype FieldName = FieldName  String
  deriving newtype  (Show, Eq, Ord)
  deriving anyclass IsName

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

{-------------------------------------------------------------------------------
  Our view of a record declaration
-------------------------------------------------------------------------------}


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
      recordUnqual :: TypeName

      -- | The type variables in the record type
    , recordTVars :: [TyVarBndr]

      -- | Unqualified name of the record constructor
    , recordConstr :: ConstrName

      -- | The fields in the record
    , recordFields :: [Field]

      -- | The type class instances that should be derived
    , recordDeriv :: [Deriving]
    }
  deriving (Show)

data Field = Field {
      -- | Unqualified name of the field
      fieldUnqual :: FieldName

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

-- | Try to match a record declaration
--
-- We use 'Maybe' in these matching functions, along with 'reportError', so that
-- we can report multiple errors rather than stopping at the first.
matchRecord :: Dec -> Q (Maybe Record)
matchRecord (DataD
          _cxt@[]
          typeName
          tyVarBndrs
          _kind@Nothing
          [RecC constrName fields]
          derivClauses
       ) = fmap Just $
        Record
    <$> pure (TypeName $ nameBase typeName)
    <*> pure tyVarBndrs
    <*> pure (ConstrName $ nameBase constrName)
    <*> (catMaybes <$> mapM matchField (zip [0..] fields))
    <*> concatMapM matchDeriv derivClauses
matchRecord d = do
    reportError $ "Unsupported declaration: " ++ show d
    return Nothing

-- | Support deriving clauses
--
-- TODO: We'll want to support some additional built-in classes probably,
-- and we can for sure support 'DeriveAnyClass' style derivation.
matchDeriv :: DerivClause -> Q [Deriving]
matchDeriv (DerivClause Nothing cs) =
    catMaybes <$> mapM go cs
  where
    go :: Pred -> Q (Maybe Deriving)
    go p | p == ConT ''Eq   = return $ Just DeriveEq
         | p == ConT ''Show = return $ Just DeriveShow
         | otherwise        = do
             reportError $ "Cannot derive instance for " ++ show p
             return Nothing
matchDeriv (DerivClause (Just _) _) = do
    reportError "Deriving strategies not supported"
    return []

matchField :: (Int, VarBangType) -> Q (Maybe Field)
matchField (i, (fieldName, bng, typ)) =
    case bng of
      DefaultBang ->
        return . Just $ Field (unqualify fieldName) typ i
      _otherwise  -> do
        reportError $ "Unsupported bang type: " ++ show bng
        return Nothing
  where
    unqualify :: Name -> FieldName
    unqualify = FieldName . undoDRF . nameBase

-- When @DuplicateRecordFields@ is enabled, it produces field names such as
-- @$sel:a:MkY@. We don't really care much about 'DuplicateRecordFields',
-- insofar as that we will not try to be compatible with DRF-style
-- overloading (all overloading must happen through 'HasField' instead).
-- We do however need to recover the original field name.
--
-- <https://gitlab.haskell.org/ghc/ghc/-/wikis/records/overloaded-record-fields/duplicate-record-fields>
-- <https://gitlab.haskell.org/ghc/ghc/-/issues/14848>
undoDRF :: String -> String
undoDRF fieldName =
   case fieldName of
     '$' : drf  -> takeWhile (/= ':') . tail . dropWhile (/= ':') $ drf
     _otherwise -> fieldName

pattern DefaultBang :: Bang
pattern DefaultBang = Bang NoSourceUnpackedness NoSourceStrictness

{-------------------------------------------------------------------------------
  Our view of a record value definition
-------------------------------------------------------------------------------}

data RecordValue = RecordValue {
      recordValueConstr :: ConstrName
    , recordValueFields :: Map FieldName Exp
    }
  deriving (Show)

matchRecordValue :: Exp -> Q RecordValue
matchRecordValue (RecConE constr fields) =
        RecordValue (ConstrName $ nameBase constr) . Map.fromList
    <$> mapM matchFieldValue fields
matchRecordValue e =
    fail $ "Unsupported definition: " ++ show e

matchFieldValue :: FieldExp -> Q (FieldName, Exp)
matchFieldValue (f, e) = return $ (FieldName $ nameBase f, e)

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
simpleFn fnName qTyp qBody = do
    typ  <- qTyp
    body <- qBody
    return [
          SigD fnName typ
        , ValD (VarP fnName) (NormalB body) []
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

typeLevelList :: [Q Type] -> Q Type
typeLevelList = foldr cons nil
  where
    nil       = promotedNilT
    cons t ts = promotedConsT `appT` t `appT` ts

typeLevelTuple :: [Q Type] -> Q Type
typeLevelTuple ts = appsT (promotedTupleT (length ts)) ts

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

firstToLower :: String -> String
firstToLower []     = []
firstToLower (c:cs) = toLower c : cs

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
