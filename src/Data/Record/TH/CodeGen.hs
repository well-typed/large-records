{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Code generation
module Data.Record.TH.CodeGen (largeRecord, endOfBindingGroup) where

import Data.Maybe (catMaybes)
import Data.Proxy
import Data.Vector (Vector)
import GHC.Records.Compat
import GHC.Exts (Any)
import Language.Haskell.TH
import Unsafe.Coerce (unsafeCoerce)

import qualified Data.Generics as SYB
import qualified Data.Kind     as Kind
import qualified Data.Vector   as V

import Data.Record.Generic
import Data.Record.Generic.Eq
import Data.Record.Generic.Show

import Data.Record.TH.CodeGen.TH
import Data.Record.TH.CodeGen.Tree
import Data.Record.TH.CodeGen.Util
import Data.Record.TH.CodeGen.View
import Data.Record.TH.Config.Naming
import Data.Record.TH.Config.Options
import Data.Record.TH.Runtime

import qualified Data.Record.Generic.Rep     as Rep
import qualified Data.Record.TH.CodeGen.Name as N

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
    rs <- mapM matchRecord =<< decls
    concatMapM (genAll opts) (catMaybes rs)

-- | Tell @ghc@ that the end of a binding group was reached
--
-- @ghc@ type checks declarations in groups called "binding groups".
-- Unfortunately, that doesn't always work so well when TH is involved.
-- For example:
--
-- > largeRecord defaultPureScript [d|
-- >     data T a = MkT { x :: Int, y :: [a] }
-- >   |]
-- >
-- > endOfBindingGroup
-- >
-- > projectOne :: T Bool -> Int
-- > projectOne [lr| MkT { x = a } |] = a
--
-- Without the call to 'endOfBindingGroup' here, the definition of @projectOne@
-- will result in an error about some definitions (created by the call to
-- 'largeRecord') not being in scope.
--
-- Most of the time it should not be necessary to call this function.
-- In particular, if the definition of the record is in a different module,
-- calling this function is /definitely/ not required. Even if that is not the
-- case, however, most of the time this function is not needed, but the exact
-- conditions are then a bit harder to specify (it depends on @ghc@'s binding
-- group analysis).
endOfBindingGroup :: Q [Dec]
endOfBindingGroup = return []

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

-- | Generate all definitions
genAll :: Options -> Record -> Q [Dec]
genAll opts@Options{..} r = concatM $ [
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
genNewtype :: Options -> Record -> Q Dec
genNewtype opts r@Record{..} =
    N.newtypeD
      (cxt [])
      recordUnqual
      recordTVars
      Nothing
      (N.recC (nameRecordInternalConstr opts r) [
           N.varBangType (nameRecordInternalField opts r) $
             bangType (return DefaultBang) [t| Vector Any |]
         ])
      []

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
-- > unsafeGetIndexT = \ n t -> unsafeCoerce (V.unsafeIndex (vectorFromT t) n)
genIndexedAccessor :: Options -> Record -> Q [Dec]
genIndexedAccessor opts r@Record{..} = do
    x <- newName "x"
    simpleFn
      (nameRecordIndexedAccessor opts r)
      (forallT
         (PlainTV x : recordTVars)
         (cxt [])
         (arrT [conT ''Int, recordTypeT opts r] (varT x)))
      [| \n t -> unsafeCoerce (V.unsafeIndex ($(recordToVectorE opts r) t) n) |]

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
        (arrT [conT ''Int, recordTypeT opts r, varT x] (recordTypeT opts r)))
      body
  where
    body :: Q Exp
    body
      | allFieldsStrict =
          [| \n t !val -> $(recordFromVectorDontForceE opts r) (
                 V.unsafeUpd ($(recordToVectorE opts r) t) [(n, unsafeCoerce val)]
               )
           |]
      | otherwise =
          [| \n t val -> $(recordFromVectorDontForceE opts r) (
                 V.unsafeUpd ($(recordToVectorE opts r) t) [(n, unsafeCoerce val)]
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
      (N.fromOverloaded fieldUnqual)
      (forallT recordTVars (cxt []) $
         arrT [recordTypeT opts r] (fieldTypeT opts f))
      (fieldUntypedAccessorE opts r f)

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
          N.typeLevelMetadata fieldUnqual
        , recordTypeT opts r
        , fieldTypeT  opts f
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
-- >   , unsafeCoerce tWord'
-- >   , unsafeCoerce tBool'
-- >   , unsafeCoerce tChar'
-- >   , unsafeCoerce tA'
-- >   , unsafeCoerce tListB'
-- >   ])
--
-- where the " constructor " on the @"(..)"@ is generated by
-- 'recordFromUnforcedVectorQ', so that we correctly deal with strict/non-strict
-- fields.
--
-- However, this function is slightly more general than this, generalizing over
-- the "kind of lambda" we want to construct. We use this both in
-- 'genPatSynonym' and in 'genConstructorFn'.
genRecordVal :: Options -> Record -> ([Q Pat] -> Q Exp -> Q a) -> Q a
genRecordVal opts r@Record{..} mkFn = do
    -- The constructor arguments are locally bound, and should not have the
    -- same name as the fields themselves
    vars <- mapM (N.fresh . fieldUnqual) recordFields
    mkFn (map N.varP vars) [|
        $(recordFromVectorForceStrictFieldsE opts r)
        $(vectorE qUnsafeCoerce vars)
      |]
  where
    qUnsafeCoerce :: N.Name 'N.Unique -> Q Exp
    qUnsafeCoerce x = [| unsafeCoerce $(N.varE x) |]

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
         arrT (map (fieldTypeT opts) recordFields) (recordTypeT opts r))
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
genInstanceMetadataOf :: Options -> Record -> Q Dec
genInstanceMetadataOf opts r@Record{..} = tySynInstD $
    tySynEqn
      Nothing
      [t| MetadataOf $(recordTypeT opts r) |]
      (plistT $ map fieldMetadata recordFields)
  where
    fieldMetadata :: Field -> Q Type
    fieldMetadata f@Field{..} = ptupleT [
          N.typeLevelMetadata fieldUnqual
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
genRecordView :: Options -> Record -> Q [Dec]
genRecordView opts r@Record{..} = do
    simpleFn
      (nameRecordView opts r)
      (forallT recordTVars (cxt []) $ arrT [recordTypeT opts r] viewType)
      viewBody
  where
    viewType :: Q Type
    viewType = mkTupleT (fieldTypeT opts) $ nest recordFields

    viewBody :: Q Exp
    viewBody = do
        x <- newName "x"
        lamE [varP x] $ mkTupleE (viewField x) $ nest recordFields

    -- We generate the view only if we are generating the pattern synonym,
    -- but when we do we don't generate the typed accessors, because they
    -- are instead derived from the pattern synonym by GHC. Since the synonym
    -- requires the view, we therefore use the untyped accessor here.
    viewField :: Name -> Field -> Q Exp
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
genPatSynonym :: Options -> Record -> Q [Dec]
genPatSynonym opts r@Record{..} = sequence [
      N.patSynSigD recordConstr $
        simplePatSynType
          recordTVars
          (map (fieldTypeT opts) recordFields)
          (recordTypeT opts r)
    , N.patSynD recordConstr
        (N.recordPatSyn $ map fieldUnqual recordFields)
        qDir
        matchVector
    , N.pragCompleteD [recordConstr] Nothing
    ]
  where
    matchVector :: Q Pat
    matchVector = viewP (N.varE (nameRecordView opts r)) $
        mkTupleP (N.varP . N.fromOverloaded . fieldUnqual) $
          nest recordFields

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
genConstraintsClass :: Options -> Record -> Q Dec
genConstraintsClass opts r = do
    c <- newName "c"
    k <- [t| Kind.Type -> Kind.Constraint |]
    N.classD
      (cxt [])
      (nameRecordConstraintsClass opts r)
      (recordTVars r ++ [KindedTV c k])
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
genRequiredConstraints :: Options -> Record -> Q Type -> Q Cxt
genRequiredConstraints opts Record{..} c = do
    constraints <- mapM constrainField recordFields
    return $ filter hasTypeVar constraints
  where
    constrainField :: Field -> Q Pred
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
-- >     unsafeCoerce (dictFor p (Proxy :: Proxy Word))
-- >   , unsafeCoerce (dictFor p (Proxy :: Proxy Bool))
-- >   , unsafeCoerce (dictFor p (Proxy :: Proxy Char))
-- >   , unsafeCoerce (dictFor p (Proxy :: Proxy a))
-- >   , unsafeCoerce (dictFor p (Proxy :: Proxy [b]))
-- >   ])
genDict :: Options -> Record -> Q Exp
genDict opts Record{..} = do
    p <- newName "p"
    lamE [varP p] [| Rep $(vectorE (dictForField p) recordFields) |]
  where
    dictForField :: Name -> Field -> Q Exp
    dictForField p f = [|
          unsafeCoerce (dictFor $(varE p) (Proxy :: Proxy $(fieldTypeT opts f)))
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
      (appsT (N.conT (nameRecordConstraintsClass opts r)) $
         map tyVarType recordTVars ++ [varT c])
      [ valD (N.varP (nameRecordConstraintsMethod opts r))
             (normalB (genDict opts r))
             []
      ]

-- | Generate the Constraints type family instance
--
-- Generates something like
--
-- > type Constraints (T a b) = Constraints_T a b
genInstanceConstraints :: Options -> Record -> Q Dec
genInstanceConstraints opts r@Record{..} = tySynInstD $
    tySynEqn
      Nothing
      [t| Constraints $(recordTypeT opts r) |]
      (appsT (N.conT (nameRecordConstraintsClass opts r)) $
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
        fieldExp 'recordName        $ N.termLevelMetadata recordUnqual
      , fieldExp 'recordConstructor $ N.termLevelMetadata recordConstr
      , fieldExp 'recordSize        $ litE (integerL numFields)
      , fieldExp 'recordFieldNames  $ [| Rep.unsafeFromListK $fieldNames |]
      ]
  where
    numFields :: Integer
    numFields = fromIntegral $ length recordFields

    fieldNames :: Q Exp
    fieldNames = listE $ map (N.termLevelMetadata . fieldUnqual) recordFields

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
          [t| $(conT clss) $(recordTypeT opts r) |]
          [valD (varP fn) (normalB (varE gfn)) []]

-- | Generate definition for `from` in the `Generic` instance
--
-- Generates something like
--
-- > repFromVectorStrict . vectorFromT
genFrom :: Options -> Record -> Q Exp
genFrom opts r = [| repFromVector . $(N.varE (nameRecordInternalField opts r)) |]

-- | Generate definition for `to` in the `Generic` instance
--
-- > (..) . repToVector
--
-- where the @(..)@ is generated by 'recordFromVectorForceStrictFieldsE'
-- (which will any strict fields in the vector).
genTo :: Options -> Record -> Q Exp
genTo opts r = [| $(recordFromVectorForceStrictFieldsE opts r) . repToVector |]

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
             [t| Generic $(recordTypeT opts r) |]
             [ genInstanceConstraints opts r
             , genInstanceMetadataOf  opts r
             , valD (varP 'from)     (normalB (genFrom opts r))                            []
             , valD (varP 'to)       (normalB (genTo   opts r))                            []
             , valD (varP 'dict)     (normalB (N.varE (nameRecordConstraintsMethod opts r))) []
             , valD (varP 'metadata) (normalB (genMetadata opts r))                        []
             ]
         ]
    , mapM (genDeriving opts r) recordDeriv
    ]

{-------------------------------------------------------------------------------
  Simple TH splices for records
-------------------------------------------------------------------------------}

-- | The saturated type of the record (that is, with all type vars applied)
recordTypeT :: Options -> Record -> Q Type
recordTypeT _opts Record{..} =
    appsT (conT (N.toName recordUnqual)) $ map tyVarType recordTVars

-- | Coerce the record to the underlying @Vector Any@
recordToVectorE :: Options -> Record -> Q Exp
recordToVectorE opts = N.varE . nameRecordInternalField opts

-- | Construct record from the underlying @Vector Any@
--
-- This doesn't force any elements in the vector, so this can be used if
--
-- * the record has lazy fields, or
-- * we know through other means that all values are already forced.
--
-- See also 'recordFromVectorForceE'.
recordFromVectorDontForceE :: Options -> Record -> Q Exp
recordFromVectorDontForceE opts = N.conE . nameRecordInternalConstr opts

-- | Construct record from the underlying @Vector Any@, forcing strict fields
--
-- Currently either /all/ fields are strict or /none/, so we either just force
-- all fields, or none of them.
--
-- See also 'recordFromVectorDontForceE'.
recordFromVectorForceStrictFieldsE :: Options -> Record -> Q Exp
recordFromVectorForceStrictFieldsE opts@Options{..} r
    | allFieldsStrict = [|
          (\v -> rnfVectorAny v `seq` $(recordFromVectorDontForceE opts r) v)
        |]
    | otherwise =
        recordFromVectorDontForceE opts r

-- | The (unsafe) indexed field accessor
recordIndexedAccessorE :: Options -> Record -> Q Exp
recordIndexedAccessorE opts = N.varE . nameRecordIndexedAccessor opts

-- | The (unsafe) indexed field overwrite
recordIndexedOverwriteE :: Options -> Record -> Q Exp
recordIndexedOverwriteE opts = N.varE . nameRecordIndexedOverwrite opts

{-------------------------------------------------------------------------------
  Simple TH splices for record fields
-------------------------------------------------------------------------------}

-- | Type of the field
fieldTypeT :: Options -> Field -> Q Type
fieldTypeT _opts Field{..} = return fieldType

-- | Index of the field
fieldIndexE :: Options -> Field -> Q Exp
fieldIndexE _opts Field{..} = litE . integerL $ fromIntegral fieldIndex

-- | The indexed field accessor, applied to this field
fieldUntypedAccessorE :: Options -> Record -> Field -> Q Exp
fieldUntypedAccessorE opts r f =
    [| $(recordIndexedAccessorE opts r) $(fieldIndexE opts f) |]

-- | The indexed field overwrite, applied to this field
fieldUntypedOverwriteE :: Options -> Record -> Field -> Q Exp
fieldUntypedOverwriteE opts r f =
    [| $(recordIndexedOverwriteE opts r) $(fieldIndexE opts f) |]
