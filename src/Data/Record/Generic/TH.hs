{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ViewPatterns        #-}

module Data.Record.Generic.TH (
    Options(..)
  , defaultOptions
  , largeRecord
  ) where

import Control.Monad.State (StateT)
import qualified Data.Kind as Kind
import Data.List (intercalate)
import Data.Vector (Vector)
import GHC.Exts (Any)

import qualified Control.Monad.State as StateT
import qualified Data.Vector         as V

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

{-------------------------------------------------------------------------------
  Public API
-------------------------------------------------------------------------------}

data Options = Options {
      generatePatternSynonym :: Bool
    }

defaultOptions :: Options
defaultOptions = Options {
      generatePatternSynonym = False
    }

largeRecord :: Options -> Q [Dec] -> Q [Dec]
largeRecord opts decls = do
    ds          <- decls
    (ds', errs) <- StateT.runStateT (concatMapM go ds) []
    case errs of
      []         -> return ds'
      _otherwise -> fail $ intercalate "\n    " .concat $ [
          errs
        , ["(" ++ show (length errs) ++ " errors)"]
        ]
  where
    go :: Dec -> StateT [String] Q [Dec]
    go (record -> Just r) = StateT.lift $ process opts r
    go d = failWith $ "largeRecord: unsupported " ++ show d

    failWith :: String -> StateT [String] Q [a]
    failWith err = do
        StateT.modify (++ [err])
        return []

{-------------------------------------------------------------------------------
  Generation
-------------------------------------------------------------------------------}

-- | Generate the newtype that will represent the record
--
-- Generates something like
--
-- > newtype T = TFromVector {vectorFromT :: V.Vector Any}
genNewtype :: Options -> Record -> Q Dec
genNewtype _ r =
    newtypeD
      (cxt [])
      nameType
      []
      Nothing
      (recC nameConstr [
           varBangType nameField $
             bangType (return DefaultBang) [t| Vector Any |]
         ])
      []
  where
    nameType, nameConstr, nameField :: Name
    nameType   = recordName               r
    nameConstr = recordInternalConstrName r
    nameField  = recordInternalFieldName  r

-- | Generate the indexed field accessor
--
-- Generates something like
--
-- > unsafeIndexT :: forall a_adzi. Int -> T -> a_adzi
-- > unsafeIndexT = \n t -> unsafeCoerce ((V.unsafeIndex (vectorFromT t)) n)
genIndexedAccessor :: Options -> Record -> Q [Dec]
genIndexedAccessor _opts r =
    simpleFn
      (recordIndexedAccessorName r)
      [t| forall a. Int -> $(recordTypeQ r) -> a |]
      [| \n t -> unsafeCoerce (V.unsafeIndex ($(recordToVectorQ r) t) n) |]

-- | Generate field accessors for all fields
genFieldAccessors :: Options -> Record -> Q [Dec]
genFieldAccessors opts r@Record{..} =
    concatMapM (genFieldAccessor opts r) recordFields

-- | Generate accessor for single field
--
-- Generates function such as
--
-- > tInt :: T -> Int
-- > tInt = unsafeIndexT 0
genFieldAccessor :: Options -> Record -> Field -> Q [Dec]
genFieldAccessor _opts r f = do
    simpleFn
      (fieldAccessorName f)
      [t| $(recordTypeQ r) -> $(fieldTypeQ f) |]
      (fieldUntypedAccessor r f)

-- | Generate view on the record
--
-- Generates function such as
--
-- > tupleFromT :: T -> (Int, Bool, Char)
-- > tupleFromT = \x -> (tInt x, tBool x, tChar x)
genRecordView :: Options -> Record -> Q [Dec]
genRecordView Options{..} r@Record{..} = do
    simpleFn
      (recordViewName r)
      [t| $(recordTypeQ r) -> $viewType |]
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
-- > pattern MkT :: Int -> Bool -> Char -> T
-- > pattern MkT{tInt, tBool, tChar} <- tupleFromT -> (tInt, tBool, tChar)
-- >   where
-- >     MkT x y z = TFromVector (V.fromList [
-- >           unsafeCoerce x
-- >         , unsafeCoerce y
-- >         , unsafeCoerce z
-- >         ])
-- >
-- > {-# COMPLETE MkT #-}
genPatSynonym :: Options -> Record -> Q [Dec]
genPatSynonym _opts r@Record{..} = sequence [
      patSynSigD (mkName recordConstr) $
        simplePatSynType (map fieldTypeQ recordFields) (recordTypeQ r)
    , patSynD (mkName recordConstr)
        (recordPatSyn $ map fieldAccessorName recordFields)
        qDir
        matchVector
    , pragCompleteD [mkName recordConstr] Nothing
    ]
  where
    matchVector :: Q Pat
    matchVector = viewP (varE (recordViewName r)) $
        mkTupleP (varP . fieldAccessorName) $ nest recordFields

    qDir :: Q PatSynDir
    qDir = do
        vars   <- mapM mkVarName recordFields
        explBidir . (:[]) $ clause
          (map varP vars)
          (normalB [| $(recordFromVectorQ r) $(mkVector coerce vars) |] )
          []

    coerce :: Name -> Q Exp
    coerce x = [| unsafeCoerce $(varE x) |]

    -- The constructor arguments are locally bound, and should not have the
    -- same name as the fields themselves
    mkVarName :: Field -> Q Name
    mkVarName = newName . fieldUnqual

{-
ClassD [
    AppT (VarT c_6989586621679064041) (ConT GHC.Types.Int)
  ,AppT (VarT c_6989586621679064041) (ConT GHC.Types.Bool)
  ,AppT (VarT c_6989586621679064041) (ConT GHC.Types.Char)
] Constraints_T_6989586621679064040 [KindedTV c_6989586621679064041 (AppT (AppT ArrowT StarT) ConstraintT)] [] []
-}

-- | Superclass constraints required by the constraints class and instance
--
-- Generates something like
--
-- > (c Int, c Bool, c Char)
genRequiredConstraints :: Options -> Record -> Name -> Q Cxt
genRequiredConstraints _opts Record{..} c =
    cxt $ map constrainField recordFields
  where
    constrainField :: Field -> Q Pred
    constrainField f = varT c `appT` fieldTypeQ f

-- | Generate the class we will use to instantiate 'Constraints'
--
-- Generates something like this:
--
-- > class (c Int, c Bool, c Char) => Constraints_T (c :: Type -> Constraint)
genConstraintsClass :: Options -> Record -> Q Dec
genConstraintsClass opts r = do
    c <- newName "c"
    k <- [t| Kind.Type -> Kind.Constraint |]
    classD
      (genRequiredConstraints opts r c)
      (recordConstraintsClassName r)
      [KindedTV c k]
      []
      []

-- | Generate (one and only) instance of the constraints class
--
-- Generates something like
--
-- > instance (c Int, c Bool, c Char) => Constraints_T c
genConstraintsInstance :: Options -> Record -> Q Dec
genConstraintsInstance opts r = do
    c <- newName "c"
    instanceD
      (genRequiredConstraints opts r c)
      (conT (recordConstraintsClassName r) `appT` varT c)
      []

-- | Generate the definitions required to provide the instance for 'Generic'
genGenericInstance :: Options -> Record -> Q [Dec]
genGenericInstance opts r = sequence [
      genConstraintsClass    opts r
    , genConstraintsInstance opts r
    ]

-- | Generate all definitions
process :: Options -> Record -> Q [Dec]
process opts@Options{..} r = concatM $ [
      (:[]) <$> genNewtype opts r
    , genIndexedAccessor opts r
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
  Decide naming
-------------------------------------------------------------------------------}

recordName :: Record -> Name
recordName Record{..} =
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
-- * 'recordInternalConstrName' will be 'TFromVector'
-- * 'recordInternalFieldName'  will be 'vectorFromT'
recordInternalConstrName :: Record -> Name
recordInternalConstrName Record{..} =
     mkName $ recordUnqual ++ "FromVector"

-- | The name of the newtype unwrapper used internally
--
-- See 'recordInternalConstrName' for a detailed discussion.
recordInternalFieldName :: Record -> Name
recordInternalFieldName Record{..} =
    mkName $ "vectorFrom" ++ recordUnqual

recordViewName :: Record -> Name
recordViewName Record{..} =
    mkName $ "tupleFrom" ++ recordUnqual

recordIndexedAccessorName :: Record -> Name
recordIndexedAccessorName Record{..} =
    mkName $ "unsafeIndex" ++ recordUnqual

recordConstraintsClassName :: Record -> Name
recordConstraintsClassName Record{..} =
    mkName $ "Constraints_" ++ recordUnqual

fieldAccessorName :: Field -> Name
fieldAccessorName Field{..} =
    mkName $ fieldUnqual

{-------------------------------------------------------------------------------
  Derived
-------------------------------------------------------------------------------}

recordTypeQ :: Record -> Q Type
recordTypeQ = conT . recordName

recordToVectorQ :: Record -> Q Exp
recordToVectorQ = varE . recordInternalFieldName

recordFromVectorQ :: Record -> Q Exp
recordFromVectorQ = conE . recordInternalConstrName

recordIndexedAccessorQ :: Record -> Q Exp
recordIndexedAccessorQ = varE . recordIndexedAccessorName

fieldTypeQ :: Field -> Q Type
fieldTypeQ Field{..} = return fieldType

fieldIndexQ :: Field -> Q Exp
fieldIndexQ Field{..} = litE . integerL $ fromIntegral fieldIndex

fieldUntypedAccessor :: Record -> Field -> Q Exp
fieldUntypedAccessor r f = [| $(recordIndexedAccessorQ r) $(fieldIndexQ f) |]

{-------------------------------------------------------------------------------
  Supported declarations

  TODOs:

  * Support type variables
  * Support (certain) deriving clauses
  * Be explicit about strictness
  * ...
-------------------------------------------------------------------------------}

-- | Unqualified name
type Unqual = String

data Record = Record {
      recordUnqual :: Unqual
    , recordConstr :: Unqual
    , recordFields :: [Field]
    }

data Field = Field {
      fieldUnqual :: Unqual
    , fieldType   :: Type
    , fieldIndex  :: Int
    }

record :: Dec -> Maybe Record
record (DataD
          _cxt@[]
          name
          _tyVarBndrs@[]
          _kind@Nothing
          [RecC constrName fields]
          _derivClauses@[]
       ) =
        Record (nameBase name) (nameBase constrName)
    <$> mapM field (zip [0..] fields)
record _otherwise =
    Nothing

field :: (Int, VarBangType)-> Maybe Field
field (i, (name, DefaultBang, typ)) = return $ Field (nameBase name) typ i
field _otherwise = Nothing

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
                         ts  -> foldl appT (tupleT (length ts)) ts
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
    limit = 2 -- 62 TODO: Set back to 62 once everything is working

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
--
-- TODO: Add support for type variables (but only universals, not existentials)
simplePatSynType :: [Q Type] -> Q Type -> Q PatSynType
simplePatSynType qFieldTypes qResultType = do
    fieldTypes <- sequence qFieldTypes
    resultType <- qResultType
    return
      $ ForallT [] []
      $ ForallT [] []
      $ foldr (\a b -> (ArrowT `AppT` a) `AppT` b) resultType fieldTypes

mkVector :: (a -> Q Exp) -> [a] -> Q Exp
mkVector f elems = [| V.fromList $(listE (map f elems)) |]

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
