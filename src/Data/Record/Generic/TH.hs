{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ViewPatterns        #-}

module Data.Record.Generic.TH (
    Options(..)
  , defaultStrictOptions
  , defaultLazyOptions
  , largeRecord
  ) where

import Control.Monad.Except (Except, runExcept, throwError)
import Control.Monad.State (StateT, runStateT)
import Data.Coerce (coerce)
import Data.Generics (everything, mkQ)
import Data.List (intercalate)
import Data.Proxy
import Data.Vector (Vector)
import GHC.Exts (Any)
import GHC.Records.Compat (HasField(..))
import Unsafe.Coerce (unsafeCoerce)

import qualified Control.Monad.State as StateT
import qualified Data.Kind           as Kind
import qualified Data.Vector         as V

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Data.Record.Generic
import Data.Record.Generic.Eq
import Data.Record.Generic.Show

import qualified Data.Record.Generic.Rep as Rep

{-------------------------------------------------------------------------------
  Options
-------------------------------------------------------------------------------}

data Options = Options {
      -- | Generate a pattern synonym for the record
      --
      -- This makes normal pattern matching on the record possible, but comes
      -- at a cost: @ghc@ will "very helpfully" introduce field accessors for
      -- all fields in the pattern synonym, which come with quadratic costs
      -- in code size.
      --
      -- Avoid if possible.
      generatePatternSynonym :: Bool

      -- | Make all fields strict
      --
      -- This should be used when using the @StrictData@ or @Strict@ language
      -- extension.
    , allFieldsStrict :: Bool
    }

defaultLazyOptions :: Options
defaultLazyOptions = Options {
      generatePatternSynonym = False
    , allFieldsStrict        = False
    }

defaultStrictOptions :: Options
defaultStrictOptions = Options {
      generatePatternSynonym = False
    , allFieldsStrict        = True
    }

{-------------------------------------------------------------------------------
  Public API
-------------------------------------------------------------------------------}

largeRecord :: Options -> Q [Dec] -> Q [Dec]
largeRecord opts decls = do
    ds          <- decls
    (ds', errs) <- runStateT (concatMapM go ds) []
    case errs of
      []         -> return ds'
      _otherwise -> fail $ intercalate "\n    " .concat $ [
          errs
        , ["(" ++ show (length errs) ++ " errors)"]
        ]
  where
    go :: Dec -> StateT [String] Q [Dec]
    go d =
        case runExcept $ matchRecord d of
          Right r   -> StateT.lift $ genAll opts r
          Left  err -> StateT.modify (++ [err]) >> return []

-- | Generate all definitions
genAll :: Options -> Record -> Q [Dec]
genAll opts@Options{..} r = concatM $ [
      (:[]) <$> genNewtype opts r
    , genIndexedAccessor   opts r
    , genIndexedOverwrite  opts r
    , genHasFieldInstances opts r
      -- If we generate the pattern synonym, there is no need to generate
      -- field accessors, because GHC will generate them from the synonym
    , when (not generatePatternSynonym) $ [
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
genNewtype _ r@Record{..} =
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
    nameType   = nameRecord               r
    nameConstr = nameRecordInternalConstr r
    nameField  = nameRecordInternalField  r

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
genIndexedAccessor _opts r@Record{..} = do
    x <- newName "x"
    simpleFn
      (nameRecordIndexedAccessor r)
      (forallT
         (PlainTV x : recordTVars)
         (cxt [])
         (fnQ [conT ''Int, recordTypeQ r] (varT x)))
      [| \n t -> unsafeCoerce (V.unsafeIndex ($(recordToVectorQ r) t) n) |]

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
genIndexedOverwrite Options{..} r@Record{..} = do
    x <- newName "x"
    simpleFn
      (nameRecordIndexedOverwrite r)
      (forallT
        (PlainTV x : recordTVars)
        (cxt [])
        (fnQ [conT ''Int, recordTypeQ r, varT x] (recordTypeQ r)))
      body
  where
    body :: Q Exp
    body
      | allFieldsStrict =
          [| \n t !val -> $(recordFromVectorQ r) (
                 V.unsafeUpd ($(recordToVectorQ r) t) [(n, unsafeCoerce val)]
               )
           |]
      | otherwise =
          [| \n t val -> $(recordFromVectorQ r) (
                 V.unsafeUpd ($(recordToVectorQ r) t) [(n, unsafeCoerce val)]
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
genFieldAccessor _opts r@Record{..} f = do
    simpleFn
      (nameFieldAccessor f)
      (forallT recordTVars (cxt []) $ fnQ [recordTypeQ r] (fieldTypeQ f))
      (fieldUntypedAccessor r f)

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
genHasFieldInstance _opts r f@Field{..} = do
    instanceD
      (cxt [])
      (appsT (conT ''HasField) [
          litT (strTyLit fieldUnqual)
        , recordTypeQ r
        , fieldTypeQ f
        ])
      [valD (varP 'hasField) (normalB [|
          \t -> ( $(fieldUntypedOverwrite r f) t
                , $(fieldUntypedAccessor  r f) t
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
genRecordView _opts r@Record{..} = do
    simpleFn
      (nameRecordView r)
      (forallT recordTVars (cxt []) $ fnQ [recordTypeQ r] viewType)
      viewBody
  where
    viewType :: Q Type
    viewType = mkTupleT fieldTypeQ $ nest recordFields

    viewBody :: Q Exp
    viewBody = do
        x <- newName "x"
        lamE [varP x] $ mkTupleE (viewField x) $ nest recordFields

    -- We generate the view only if we are generating the pattern synonym,
    -- but when we do we don't generate the typed accessors, because they
    -- are instead derived from the pattern synonym by GHC. Since the synonym
    -- requires the view, we therefore use the untyped accessor here.
    viewField :: Name -> Field -> Q Exp
    viewField x f = [| $(fieldUntypedAccessor r f) $(varE x) |]

-- | Generate pattern synonym
--
-- Constructs something like this:
--
-- > pattern MkT :: forall a b. Word -> Bool -> Char -> a -> [b] -> T a b
-- > pattern MkT{tWord, tBool, tChar, tA, tListB} <-
-- >     (tupleFromT -> (tWord, tBool, tChar, tA, tListB) )
-- >   where
-- >     MkT tWord' tBool' tChar' tA' tListB' = TFromVector (V.fromList [
-- >         , unsafeCoerce tWord'
-- >         , unsafeCoerce tBool'
-- >         , unsafeCoerce tChar'
-- >         , unsafeCoerce tA'
-- >         , unsafeCoerce tListB'
-- >         ])
-- >
-- > {-# COMPLETE MkT #-}
--
-- Modulo nesting ('nest').
genPatSynonym :: Options -> Record -> Q [Dec]
genPatSynonym _opts r@Record{..} = sequence [
      patSynSigD (mkName recordConstr) $
        simplePatSynType
          recordTVars
          (map fieldTypeQ recordFields)
          (recordTypeQ r)
    , patSynD (mkName recordConstr)
        (recordPatSyn $ map nameFieldAccessor recordFields)
        qDir
        matchVector
    , pragCompleteD [mkName recordConstr] Nothing
    ]
  where
    matchVector :: Q Pat
    matchVector = viewP (varE (nameRecordView r)) $
        mkTupleP (varP . nameFieldAccessor) $ nest recordFields

    qDir :: Q PatSynDir
    qDir = do
        vars   <- mapM mkVarName recordFields
        explBidir . (:[]) $ clause
          (map varP vars)
          (normalB [| $(recordFromVectorQ r) $(mkVector qUnsafeCoerce vars) |] )
          []

    qUnsafeCoerce :: Name -> Q Exp
    qUnsafeCoerce x = [| unsafeCoerce $(varE x) |]

    -- The constructor arguments are locally bound, and should not have the
    -- same name as the fields themselves
    mkVarName :: Field -> Q Name
    mkVarName = newName . fieldUnqual

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
genConstraintsClass _opts r = do
    c <- newName "c"
    k <- [t| Kind.Type -> Kind.Constraint |]
    classD
      (cxt [])
      (nameRecordConstraintsClass r)
      (recordTVars r ++ [KindedTV c k])
      []
      [ sigD (nameRecordConstraintsMethod r)
             [t| Proxy $(varT c) -> Rep (Dict $(varT c)) $(recordTypeQ r) |]
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
genRequiredConstraints _opts Record{..} c = do
    constraints <- mapM constrainField recordFields
    return $ filter hasTypeVar constraints
  where
    constrainField :: Field -> Q Pred
    constrainField f = c `appT` fieldTypeQ f

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
genDict _opts Record{..} = do
    p <- newName "p"
    lamE [varP p] [| Rep $(mkVector (dictForField p) recordFields) |]
  where
    dictForField :: Name -> Field -> Q Exp
    dictForField p f = [|
          unsafeCoerce (dictFor $(varE p) (Proxy :: Proxy $(fieldTypeQ f)))
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
      (appsT (conT (nameRecordConstraintsClass r)) $
         map tyVarType recordTVars ++ [varT c])
      [ valD (varP (nameRecordConstraintsMethod r))
             (normalB (genDict opts r))
             []
      ]

-- | Generate the Constraints type family instance
--
-- Generates something like
--
-- > type Constraints (T a b) = Constraints_T a b
genConstraintsFamilyInstance :: Options -> Record -> Q Dec
genConstraintsFamilyInstance _opts r@Record{..} = tySynInstD $
    tySynEqn
      Nothing
      [t| Constraints $(recordTypeQ r) |]
      (appsT (conT (nameRecordConstraintsClass r)) $ map tyVarType recordTVars)

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
      , fieldExp 'recordFieldNames  $ [| Rep.unsafeFromListK $fieldNames |]
      ]
  where
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
          [t| $(conT clss) $(recordTypeQ r) |]
          [valD (varP fn) (normalB (varE gfn)) []]

-- | Generate definition for `from` in the `Generic` instance
--
-- Generates something like
--
-- > repFromVectorStrict . vectorFromT
genFrom :: Options -> Record -> Q Exp
genFrom _opts r = [| repFromVector . $(varE (nameRecordInternalField r)) |]

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
genTo Options{..} r
  | allFieldsStrict = [| $(conE (nameRecordInternalConstr r)) . repToVectorStrict |]
  | otherwise       = [| $(conE (nameRecordInternalConstr r)) . repToVectorLazy   |]

-- | Generate the definitions required to provide the instance for 'Generic'
--
-- > instance Generic T where
-- >   type Constraints T = Constraints_T
-- >   from       = coerce
-- >   to         = coerce
-- >   recordSize = const 3
-- >   dict       = dictConstraints_T
-- >   metadata   = ..
genGenericInstance :: Options -> Record -> Q [Dec]
genGenericInstance opts r@Record{..} = concatM [
       sequence [
           genConstraintsClass         opts r
         , genConstraintsClassInstance opts r
         , instanceD
             (cxt [])
             [t| Generic $(recordTypeQ r) |]
             [ genConstraintsFamilyInstance opts r
              , valD (varP 'from)       (normalB (genFrom opts r))                       []
              , valD (varP 'to)         (normalB (genTo   opts r))                       []
              , valD (varP 'recordSize) (normalB [| const $numFields |])                 []
              , valD (varP 'dict)       (normalB (varE (nameRecordConstraintsMethod r))) []
              , valD (varP 'metadata)   (normalB (genMetadata opts r))                   []
             ]
         ]
    , mapM (genDeriving opts r) recordDeriv
    ]
  where
    numFields :: Q Exp
    numFields = litE . integerL . fromIntegral $ length recordFields

{-------------------------------------------------------------------------------
  Decide naming
-------------------------------------------------------------------------------}

nameRecord :: Record -> Name
nameRecord Record{..} =
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
nameRecordInternalConstr :: Record -> Name
nameRecordInternalConstr Record{..} =
     mkName $ recordUnqual ++ "FromVector"

-- | The name of the newtype unwrapper used internally
--
-- See 'nameRecordInternalConstr' for a detailed discussion.
nameRecordInternalField :: Record -> Name
nameRecordInternalField Record{..} =
    mkName $ "vectorFrom" ++ recordUnqual

nameRecordView :: Record -> Name
nameRecordView Record{..} =
    mkName $ "tupleFrom" ++ recordUnqual

nameRecordIndexedAccessor :: Record -> Name
nameRecordIndexedAccessor Record{..} =
    mkName $ "unsafeGetIndex" ++ recordUnqual

nameRecordIndexedOverwrite :: Record -> Name
nameRecordIndexedOverwrite Record{..} =
    mkName $ "unsafeSetIndex" ++ recordUnqual

nameRecordConstraintsClass :: Record -> Name
nameRecordConstraintsClass Record{..} =
    mkName $ "Constraints_" ++ recordUnqual

nameRecordConstraintsMethod :: Record -> Name
nameRecordConstraintsMethod Record{..} =
    mkName $ "dictConstraints_" ++ recordUnqual

nameFieldAccessor :: Field -> Name
nameFieldAccessor Field{..} =
    mkName $ fieldUnqual

{-------------------------------------------------------------------------------
  Derived
-------------------------------------------------------------------------------}

recordTypeQ :: Record -> Q Type
recordTypeQ r@Record{..} =
    appsT (conT (nameRecord r)) $ map tyVarType recordTVars

recordToVectorQ :: Record -> Q Exp
recordToVectorQ = varE . nameRecordInternalField

recordFromVectorQ :: Record -> Q Exp
recordFromVectorQ = conE . nameRecordInternalConstr

recordIndexedAccessorQ :: Record -> Q Exp
recordIndexedAccessorQ = varE . nameRecordIndexedAccessor

recordIndexedOverwriteQ :: Record -> Q Exp
recordIndexedOverwriteQ = varE . nameRecordIndexedOverwrite

fieldTypeQ :: Field -> Q Type
fieldTypeQ Field{..} = return fieldType

fieldIndexQ :: Field -> Q Exp
fieldIndexQ Field{..} = litE . integerL $ fromIntegral fieldIndex

fieldUntypedAccessor :: Record -> Field -> Q Exp
fieldUntypedAccessor r f = [| $(recordIndexedAccessorQ r) $(fieldIndexQ f) |]

fieldUntypedOverwrite :: Record -> Field -> Q Exp
fieldUntypedOverwrite r f = [| $(recordIndexedOverwriteQ r) $(fieldIndexQ f) |]

{-------------------------------------------------------------------------------
  Supported declarations
-------------------------------------------------------------------------------}

-- | Unqualified name
type Unqual = String

-- TODO: Reorder and give an example
data Record = Record {
      recordUnqual :: Unqual
    , recordConstr :: Unqual
    , recordFields :: [Field]
    , recordDeriv  :: [Deriving]
    , recordTVars  :: [TyVarBndr]
    }

data Field = Field {
      fieldUnqual :: Unqual
    , fieldType   :: Type
    , fieldIndex  :: Int
    }

data Deriving =
    DeriveEq
  | DeriveShow

matchRecord :: Dec -> Except String Record
matchRecord (DataD
          _cxt@[]
          name
          tyVarBndrs
          _kind@Nothing
          [RecC constrName fields]
          derivClauses
       ) =
        Record
    <$> pure (nameBase name)
    <*> pure (nameBase constrName)
    <*> mapM matchField (zip [0..] fields)
    <*> concatMapM matchDeriv derivClauses
    <*> pure tyVarBndrs
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
      DefaultBang -> return $ Field (nameBase name) typ i
      _otherwise  -> throwError $ "Unsupported bang type: " ++ show bng

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
