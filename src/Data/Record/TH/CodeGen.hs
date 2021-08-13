{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TupleSections    #-}

-- | Code generation
module Data.Record.TH.CodeGen (
    largeRecord
  ) where

import Data.List (nub)
import Data.Maybe (catMaybes)
import Data.Proxy
import Data.Vector (Vector)
import GHC.Exts (Any)
import GHC.Records.Compat
import Language.Haskell.TH

import qualified GHC.Generics  as GHC
import qualified Data.Generics as SYB
import qualified Data.Kind     as Kind
import qualified Data.Vector   as V

import Data.Record.Generic
import Data.Record.Generic.Eq
import Data.Record.Generic.GHC
import Data.Record.Generic.Show

import Data.Record.Internal.RecordInfo (fromRecordDef)
import Data.Record.Internal.RecordDef
import Data.Record.Internal.TH.Util
import Data.Record.Internal.Util
import Data.Record.Internal.RecordInfo.Resolution.Internal (putRecordInfo)
import Data.Record.TH.CodeGen.Tree
import Data.Record.TH.Config.Naming
import Data.Record.TH.Config.Options
import Data.Record.TH.Runtime

import qualified Data.Record.Generic.Rep.Internal as Rep
import qualified Data.Record.Internal.TH.Name as N

{-------------------------------------------------------------------------------
  Public API
-------------------------------------------------------------------------------}

-- | Declare a large record
--
-- Example usage:
--
-- > largeRecord defaultPureScript [d|
-- >     data R a = MkR { x :: Int, y :: [a] } deriving (Eq, Show)
-- >     data S a = S   { x :: Int, y :: [a] } deriving (Eq, Show)
-- >   |]
largeRecord :: Options -> Q [Dec] -> Q [Dec]
largeRecord opts decls = do
    rs <- mapM parseRecordDef =<< decls
    concatMapM (genAll opts) (catMaybes rs)

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

-- | Generate all definitions
genAll :: Options -> RecordDef -> Q [Dec]
genAll opts@Options{..} r = do
    putRecordInfo =<< fromRecordDef r
    concatM $ [
        (:[]) <$> genNewtype opts r
      , genIndexedAccessor   opts r
      , genIndexedOverwrite  opts r
      , when generateHasFieldInstances $ [
            genHasFieldInstances opts r
          ]
        -- If we generate the pattern synonym, there is no need to generate
        -- field accessors, because GHC will generate them from the synonym
      , when (generateFieldAccessors && not generatePatternSynonym) $ [
            genFieldAccessors opts r
          ]
      , when generateConstructorFn [
            genConstructorFn opts r
          ]
      , when generatePatternSynonym $ [
            genRecordView opts r
          , genPatSynonym opts r
          ]
      , genGenericInstance opts r
      , genGhcGenericsInstances opts r
      ]
  where
    when :: Bool -> [Q [Dec]] -> Q [Dec]
    when False _   = return []
    when True  gen = concatM gen

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
-- >   deriving anyclass C -- where applicable
genNewtype :: Options -> RecordDef -> Q Dec
genNewtype opts r@RecordDef{..} =
    N.newtypeD
      (cxt [])
      recordDefUnqual
      recordDefTVars
      Nothing
      (N.recC (nameRecordInternalConstr opts r) [
           N.varBangType (nameRecordInternalField opts r) $
             bangType (return DefaultBang) [t| Vector Any |]
         ])
      (map anyclassDerivClause recordDefAnyclass)
  where
    anyclassDerivClause :: Type -> DerivClauseQ
    anyclassDerivClause clss = derivClause (Just AnyclassStrategy) [pure clss]

{-------------------------------------------------------------------------------
  Generation: field accessors

  TODO: If we had support within GHC itself for accessing fields in records,
  we might be able to integrate this a lot more closely with normal GHC,
  especially when combined with the @NoFieldSelectors@ extension.

  See <https://gitlab.haskell.org/ghc/ghc/-/issues/17991>
-------------------------------------------------------------------------------}

-- | Generate the indexed field accessor
--
-- Generates something like
--
-- > unsafeGetIndexT :: forall x a b. Int -> T a b -> x
-- > unsafeGetIndexT = \ n t -> noInlineUnsafeCo (V.unsafeIndex (vectorFromT t) n)

genIndexedAccessor :: Options -> RecordDef -> Q [Dec]
genIndexedAccessor opts r@RecordDef{..} = do
    x <- newName "x"
    simpleFn
      (nameRecordIndexedAccessor opts r)
      (forallT
         (PlainTV x : recordDefTVars)
         (cxt [])
         (arrT [conT ''Int, recordTypeT opts r] (varT x)))
      [| \n t -> noInlineUnsafeCo (V.unsafeIndex ($(recordToVectorE opts r) t) n) |]

-- | Generate index field overwrite
--
-- Generates something like
--
-- > unsafeSetIndexT :: forall x a b. Int -> T a b -> x -> T a b
-- > unsafeSetIndexT = \n t val ->
-- >     TFromVector (V.unsafeUpd (vectorFromT t) [(n, noInlineUnsafeCo val)])
--
-- If using 'allFieldsStrict', the function will be strict in @val@.
--
-- TODO: We should support per-field strictness.
genIndexedOverwrite :: Options -> RecordDef -> Q [Dec]
genIndexedOverwrite opts@Options{..} r@RecordDef{..} = do
    x <- newName "x"
    simpleFn
      (nameRecordIndexedOverwrite opts r)
      (forallT
        (PlainTV x : recordDefTVars)
        (cxt [])
        (arrT [conT ''Int, recordTypeT opts r, varT x] (recordTypeT opts r)))
      body
  where
    body :: Q Exp
    body
      | allFieldsStrict =
          [| \n t !val -> $(recordFromVectorDontForceE opts r) (
                 V.unsafeUpd ($(recordToVectorE opts r) t) [(n, noInlineUnsafeCo val)]
               )
           |]
      | otherwise =
          [| \n t val -> $(recordFromVectorDontForceE opts r) (
                 V.unsafeUpd ($(recordToVectorE opts r) t) [(n, noInlineUnsafeCo val)]
               )
           |]

-- | Generate field accessors for all fields
genFieldAccessors :: Options -> RecordDef -> Q [Dec]
genFieldAccessors opts r@RecordDef{..} =
    concatMapM (genFieldAccessor opts r) recordDefFields

-- | Generate accessor for single field
--
-- Generates function such as
--
-- > tWord :: forall a b. T a b -> Word
-- > tWord = unsafeGetIndexT 0
genFieldAccessor :: Options -> RecordDef -> FieldDef -> Q [Dec]
genFieldAccessor opts r@RecordDef{..} f@FieldDef{..} = do
    simpleFn
      (N.fromOverloaded fieldDefUnqual)
      (forallT recordDefTVars (cxt []) $
         arrT [recordTypeT opts r] (fieldTypeT opts f))
      (fieldUntypedAccessorE opts r f)

-- | Generate 'HasField' instances for all fields
genHasFieldInstances :: Options -> RecordDef -> Q [Dec]
genHasFieldInstances opts r@RecordDef{..} =
    mapM (genHasFieldInstance opts r) recordDefFields

-- | Generate 'HasField' instance for single field
--
-- Generates something like
--
-- > instance x ~ Word => HasField "tInt" (T a b) x where
-- >   hasField = \t -> (unsafeSetIndexT 0 t, unsafeGetIndexT 0 t)
genHasFieldInstance :: Options -> RecordDef -> FieldDef -> Q Dec
genHasFieldInstance opts r f@FieldDef{..} = do
    requiresExtensions [
        DataKinds
      , FlexibleInstances
      , MultiParamTypeClasses
      , TypeFamilies
      , UndecidableInstances
      ]
    x <- newName "x"
    instanceD
      (cxt [equalityT `appT` varT x `appT` fieldTypeT opts f])
      (appsT (conT ''HasField) [
          N.typeLevelMetadata fieldDefUnqual
        , recordTypeT opts r
        , varT x
        ])
      [valD (varP 'hasField) (normalB [|
          \t -> ( $(fieldUntypedOverwriteE opts r f) t
                , $(fieldUntypedAccessorE  opts r f) t
                )
        |]) []]

{-------------------------------------------------------------------------------
  Generation: constructor function
-------------------------------------------------------------------------------}

-- | Construct a value of the record
--
-- Generates something like
--
-- > \tWord' tBool' tChar' tA' tListB' -> (..) (V.fromList [
-- >   , noInlineUnsafeCo tWord'
-- >   , noInlineUnsafeCo tBool'
-- >   , noInlineUnsafeCo tChar'
-- >   , noInlineUnsafeCo tA'
-- >   , noInlineUnsafeCo tListB'
-- >   ])
--
-- where the " constructor " on the @"(..)"@ is generated by
-- 'recordFromUnforcedVectorQ', so that we correctly deal with strict/non-strict
-- fields.
--
-- However, this function is slightly more general than this, generalizing over
-- the "kind of lambda" we want to construct. We use this both in
-- 'genPatSynonym' and in 'genConstructorFn'.
genRecordVal :: Options -> RecordDef -> ([Q Pat] -> Q Exp -> Q a) -> Q a
genRecordVal opts r@RecordDef{..} mkFn = do
    -- The constructor arguments are locally bound, and should not have the
    -- same name as the fields themselves
    vars <- mapM (N.fresh . fieldDefUnqual) recordDefFields
    mkFn (map N.varP vars) [|
        $(recordFromVectorForceStrictFieldsE opts r)
        $(vectorE qNoInlineUnsafeCo vars)
      |]
  where
    qNoInlineUnsafeCo :: N.Name 'N.VarName 'N.Unique -> Q Exp
    qNoInlineUnsafeCo x = [| noInlineUnsafeCo $(N.varE x) |]

-- | Generate constructor function
--
-- Generates something like
--
-- > mkT :: forall a b. Word -> Bool -> Char -> a -> [b] -> T a b
-- > mkT = ..
--
-- where the body of @mkT@ is generated by 'genRecordVal'.
genConstructorFn :: Options -> RecordDef -> Q [Dec]
genConstructorFn opts r@RecordDef{..} = do
    simpleFn
      (constructNameConstructorFn recordDefConstr)
      (forallT recordDefTVars (cxt []) $
         arrT (map (fieldTypeT opts) recordDefFields) (recordTypeT opts r))
      (genRecordVal opts r lamE)

{-------------------------------------------------------------------------------
  Generation: type-level metadata
-------------------------------------------------------------------------------}

-- | Generate type-level metadata
--
-- Generates something like
--
-- > type MetadataOf (T a b) = '[
-- >     '("tInt", Word),
-- >   , '("tBool", Bool),
-- >   , '("tChar", Char),
-- >   , '("tA", a),
-- >   , '("tListB", [b])
-- >   ]
--
-- NOTE: We do not use type-level lists in most places, since it's difficult
-- to avoid quadratic core code size when working with type-level list. We use
-- this meta-data currently for two purposes only:
--
-- * The 'lr' quasi-quoter uses it as a way to lookup the record definition.
--   See "Data.Record.Internal.RecordInfo.Resolution.GHC".
-- * We use it to put a constraint on 'normalize'; this constraint is carefully
--   defined to avoid quadratic core code size.
--   See "Data.Record.Generic.Transform".
genInstanceMetadataOf :: Options -> RecordDef -> Q Dec
genInstanceMetadataOf opts r@RecordDef{..} = tySynInstD $
    tySynEqn
      Nothing
      [t| MetadataOf $(recordTypeT opts r) |]
      (plistT $ map fieldMetadata recordDefFields)
  where
    fieldMetadata :: FieldDef -> Q Type
    fieldMetadata f@FieldDef{..} = ptupleT [
          N.typeLevelMetadata fieldDefUnqual
        , fieldTypeT opts f
        ]

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
genRecordView :: Options -> RecordDef -> Q [Dec]
genRecordView opts r@RecordDef{..} = do
    simpleFn
      (nameRecordView opts r)
      (forallT recordDefTVars (cxt []) $ arrT [recordTypeT opts r] viewType)
      viewBody
  where
    viewType :: Q Type
    viewType = mkTupleT (fieldTypeT opts) $
                 nest DefaultGhcTupleLimit recordDefFields

    viewBody :: Q Exp
    viewBody = do
        x <- newName "x"
        lamE [varP x] $ mkTupleE (viewField x) $
          nest DefaultGhcTupleLimit recordDefFields

    -- We generate the view only if we are generating the pattern synonym,
    -- but when we do we don't generate the typed accessors, because they
    -- are instead derived from the pattern synonym by GHC. Since the synonym
    -- requires the view, we therefore use the untyped accessor here.
    viewField :: Name -> FieldDef -> Q Exp
    viewField x f = [| $(fieldUntypedAccessorE opts r f) $(varE x) |]

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
genPatSynonym :: Options -> RecordDef -> Q [Dec]
genPatSynonym opts r@RecordDef{..} = do
    requiresExtensions [PatternSynonyms, ViewPatterns]
    sequence [
        N.patSynSigD recordDefConstr $
          simplePatSynType
            recordDefTVars
            (map (fieldTypeT opts) recordDefFields)
            (recordTypeT opts r)
      , N.patSynD recordDefConstr
          (N.recordPatSyn $ map fieldDefUnqual recordDefFields)
          qDir
          matchVector
      , N.pragCompleteD [recordDefConstr] Nothing
      ]
  where
    matchVector :: Q Pat
    matchVector = viewP (N.varE (nameRecordView opts r)) $
        mkTupleP (N.varP . N.fromOverloaded . fieldDefUnqual) $
          nest DefaultGhcTupleLimit recordDefFields

    constrVector :: [Q Pat] -> Q Exp -> Q Clause
    constrVector xs body = clause xs (normalB body) []

    qDir :: Q PatSynDir
    qDir = explBidir . (:[]) $ genRecordVal opts r constrVector

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
genConstraintsClass :: Options -> RecordDef -> Q Dec
genConstraintsClass opts r = do
    requiresExtensions [KindSignatures, ConstraintKinds]
    c <- newName "c"
    k <- [t| Kind.Type -> Kind.Constraint |]
    N.classD
      (cxt [])
      (nameRecordConstraintsClass opts r)
      (recordDefTVars r ++ [KindedTV c k])
      []
      [ N.sigD (nameRecordConstraintsMethod opts r) [t|
            Proxy $(varT c) -> Rep (Dict $(varT c)) $(recordTypeT opts r)
          |]
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
genRequiredConstraints :: Options -> RecordDef -> Q Type -> Q Cxt
genRequiredConstraints opts RecordDef{..} c = do
    requiresExtensions [FlexibleContexts]
    constraints <- mapM constrainField recordDefFields
    return $ nub $ filter hasTypeVar constraints
  where
    constrainField :: FieldDef -> Q Pred
    constrainField f = c `appT` fieldTypeT opts f

    hasTypeVar :: Pred -> Bool
    hasTypeVar = SYB.everything (||) (SYB.mkQ False isTypeVar)

    isTypeVar :: Type -> Bool
    isTypeVar (VarT _)   = True
    isTypeVar _otherwise = False

-- | Generate the dictionary creation function ('dict')
--
-- Generates something like
--
-- > \p -> Rep (V.fromList [
-- >     noInlineUnsafeCo (dictFor p (Proxy :: Proxy Word))
-- >   , noInlineUnsafeCo (dictFor p (Proxy :: Proxy Bool))
-- >   , noInlineUnsafeCo (dictFor p (Proxy :: Proxy Char))
-- >   , noInlineUnsafeCo (dictFor p (Proxy :: Proxy a))
-- >   , noInlineUnsafeCo (dictFor p (Proxy :: Proxy [b]))
-- >   ])
genDict :: Options -> RecordDef -> Q Exp
genDict opts RecordDef{..} = do
    p <- newName "p"
    lamE [varP p] [| Rep $(vectorE (dictForField p) recordDefFields) |]
  where
    dictForField :: Name -> FieldDef -> Q Exp
    dictForField p f = [|
          noInlineUnsafeCo (dictFor $(varE p) (Proxy :: Proxy $(fieldTypeT opts f)))
        |]

-- | Generate (one and only) instance of the constraints class
--
-- Generates something like
--
-- > instance (..) => Constraints_T a b c where
-- >   dictConstraints_T = ..
--
-- where the body of @dictConstraints_T@ is generated by 'genDict'.
genConstraintsClassInstance :: Options -> RecordDef -> Q Dec
genConstraintsClassInstance opts r@RecordDef{..} = do
    requiresExtensions [ScopedTypeVariables]
    c <- newName "c"
    instanceD
      (genRequiredConstraints opts r (varT c))
      (appsT (N.conT (nameRecordConstraintsClass opts r)) $
         map tyVarType recordDefTVars ++ [varT c])
      [ valD (N.varP (nameRecordConstraintsMethod opts r))
             (normalB (genDict opts r))
             []
      ]

-- | Generate the Constraints type family instance
--
-- Generates something like
--
-- > type Constraints (T a b) = Constraints_T a b
genInstanceConstraints :: Options -> RecordDef -> Q Dec
genInstanceConstraints opts r@RecordDef{..} = tySynInstD $
    tySynEqn
      Nothing
      [t| Constraints $(recordTypeT opts r) |]
      (appsT (N.conT (nameRecordConstraintsClass opts r)) $
         map tyVarType recordDefTVars)

-- | Generate metadata
--
-- Generates something like
--
-- > \_p  -> Metadata {
-- >     recordName          = "T"
-- >   , recordConstructor   = "MkT"
-- >   , recordSize          = 5
-- >   , recordFieldMetadata = Rep $ Data.Vector.fromList [
-- >         FieldMetadata (Proxy :: Proxy "tInt"))   FieldLazy
-- >       , FieldMetadata (Proxy :: Proxy "tBool"))  FieldLazy
-- >       , FieldMetadata (Proxy :: Proxy "tChar"))  FieldLazy
-- >       , FieldMetadata (Proxy :: Proxy "tA"))     FieldLazy
-- >       , FieldMetadata (Proxy :: Proxy "tListB")) FieldLazy
-- >       ]
-- >   }
genMetadata :: Options -> RecordDef -> Q Exp
genMetadata Options{..} RecordDef{..} = do
    p <- newName "_p"
    lamE [varP p] $ recConE 'Metadata [
        fieldExp 'recordName          $ N.termLevelMetadata recordDefUnqual
      , fieldExp 'recordConstructor   $ N.termLevelMetadata recordDefConstr
      , fieldExp 'recordSize          $ litE (integerL numFields)
      , fieldExp 'recordFieldMetadata $ [| Rep.Rep $ V.fromList $fieldMetadata |]
      ]
  where
    numFields :: Integer
    numFields = fromIntegral $ length recordDefFields

    fieldMetadata :: Q Exp
    fieldMetadata = listE $ map (mkFieldMetadata . fieldDefUnqual) recordDefFields

    mkFieldMetadata :: N.OverloadedName -> ExpQ
    mkFieldMetadata n = [|
          FieldMetadata
            (Proxy :: Proxy $(N.typeLevelMetadata n) )
            $(if allFieldsStrict
                then [| FieldStrict |]
                else [| FieldLazy   |])
        |]

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
genDeriving :: Options -> RecordDef -> Deriving -> Q Dec
genDeriving opts r = \case
    DeriveEq    -> inst ''Eq   '(==)      'geq
    DeriveOrd   -> inst ''Ord  'compare   'gcompare
    DeriveShow  -> inst ''Show 'showsPrec 'gshowsPrec
  where
    inst :: Name -> Name -> Name -> Q Dec
    inst clss fn gfn =
        instanceD
          (genRequiredConstraints opts r (conT clss))
          [t| $(conT clss) $(recordTypeT opts r) |]
          [valD (varP fn) (normalB (varE gfn)) []]

-- | Generate definition for `from` in the `Generic` instance
--
-- Generates something like
--
-- > repFromVectorStrict . vectorFromT
genFrom :: Options -> RecordDef -> Q Exp
genFrom opts r = [| repFromVector . $(N.varE (nameRecordInternalField opts r)) |]

-- | Generate definition for `to` in the `Generic` instance
--
-- > (..) . repToVector
--
-- where the @(..)@ is generated by 'recordFromVectorForceStrictFieldsE'
-- (which will any strict fields in the vector).
genTo :: Options -> RecordDef -> Q Exp
genTo opts r = [| $(recordFromVectorForceStrictFieldsE opts r) . repToVector |]

-- | Generate the definitions required to provide the instance for 'Generic'
--
-- > instance Generic T where
-- >   type Constraints T = Constraints_T
-- >   from       = coerce
-- >   to         = coerce
-- >   dict       = dictConstraints_T
-- >   metadata   = ..
genGenericInstance :: Options -> RecordDef -> Q [Dec]
genGenericInstance opts r@RecordDef{..} = concatM [
       sequence [
           genConstraintsClass         opts r
         , genConstraintsClassInstance opts r
         , instanceD
             (cxt [])
             [t| Generic $(recordTypeT opts r) |]
             [ genInstanceConstraints opts r
             , genInstanceMetadataOf  opts r
             , valD (varP 'from)     (normalB (genFrom opts r))                            []
             , valD (varP 'to)       (normalB (genTo   opts r))                            []
             , valD (varP 'dict)     (normalB (N.varE (nameRecordConstraintsMethod opts r))) []
             , valD (varP 'metadata) (normalB (genMetadata opts r))                        []
             ]
         ]
    , mapM (genDeriving opts r) recordDefDeriv
    ]

{-------------------------------------------------------------------------------
  GHC generics
-------------------------------------------------------------------------------}

-- | Generate GHC generics instance
--
-- Generates something like
--
-- > instance GHC.Generic ExampleRecord where
-- >   type Rep ExampleRecord = ThroughLRGenerics ExampleRecord
-- >
-- >   from = WrapThroughLRGenerics
-- >   to   = unwrapThroughLRGenerics
--
-- See 'ThroughLRGenerics' for documentation.

genGhcGenericsInstances :: Options -> RecordDef -> Q [Dec]
genGhcGenericsInstances opts r = sequenceA [
      instanceD
        (cxt [])
        [t| GHC.Generic $(recordTypeT opts r) |]
        [ tySynInstD $
            tySynEqn
              Nothing
              [t| GHC.Rep $(recordTypeT opts r) |]
              [t| ThroughLRGenerics $(recordTypeT opts r) |]
        , valD (varP 'GHC.from) (normalB (conE 'WrapThroughLRGenerics))   []
        , valD (varP 'GHC.to)   (normalB (varE 'unwrapThroughLRGenerics)) []
        ]
    ]

{-------------------------------------------------------------------------------
  Simple TH splices for records
-------------------------------------------------------------------------------}

-- | The saturated type of the record (that is, with all type vars applied)
recordTypeT :: Options -> RecordDef -> Q Type
recordTypeT _opts RecordDef{..} =
    appsT (conT (N.toName recordDefUnqual)) $ map tyVarType recordDefTVars

-- | Coerce the record to the underlying @Vector Any@
recordToVectorE :: Options -> RecordDef -> Q Exp
recordToVectorE opts = N.varE . nameRecordInternalField opts

-- | Construct record from the underlying @Vector Any@
--
-- This doesn't force any elements in the vector, so this can be used if
--
-- * the record has lazy fields, or
-- * we know through other means that all values are already forced.
--
-- See also 'recordFromVectorForceE'.
recordFromVectorDontForceE :: Options -> RecordDef -> Q Exp
recordFromVectorDontForceE opts = N.conE . nameRecordInternalConstr opts

-- | Construct record from the underlying @Vector Any@, forcing strict fields
--
-- Currently either /all/ fields are strict or /none/, so we either just force
-- all fields, or none of them.
--
-- See also 'recordFromVectorDontForceE'.
recordFromVectorForceStrictFieldsE :: Options -> RecordDef -> Q Exp
recordFromVectorForceStrictFieldsE opts@Options{..} r
    | allFieldsStrict = [|
          (\v -> rnfVectorAny v `seq` $(recordFromVectorDontForceE opts r) v)
        |]
    | otherwise =
        recordFromVectorDontForceE opts r

-- | The (unsafe) indexed field accessor
recordIndexedAccessorE :: Options -> RecordDef -> Q Exp
recordIndexedAccessorE opts = N.varE . nameRecordIndexedAccessor opts

-- | The (unsafe) indexed field overwrite
recordIndexedOverwriteE :: Options -> RecordDef -> Q Exp
recordIndexedOverwriteE opts = N.varE . nameRecordIndexedOverwrite opts

{-------------------------------------------------------------------------------
  Simple TH splices for record fields
-------------------------------------------------------------------------------}

-- | Type of the field
fieldTypeT :: Options -> FieldDef -> Q Type
fieldTypeT _opts FieldDef{..} = return fieldDefType

-- | Index of the field
fieldIndexE :: Options -> FieldDef -> Q Exp
fieldIndexE _opts FieldDef{..} = litE . integerL $ fromIntegral fieldDefIndex

-- | The indexed field accessor, applied to this field
fieldUntypedAccessorE :: Options -> RecordDef -> FieldDef -> Q Exp
fieldUntypedAccessorE opts r f =
    [| $(recordIndexedAccessorE opts r) $(fieldIndexE opts f) |]

-- | The indexed field overwrite, applied to this field
fieldUntypedOverwriteE :: Options -> RecordDef -> FieldDef -> Q Exp
fieldUntypedOverwriteE opts r f =
    [| $(recordIndexedOverwriteE opts r) $(fieldIndexE opts f) |]
