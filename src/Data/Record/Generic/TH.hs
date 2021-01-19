{-# LANGUAGE CPP                 #-}
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

import Data.Vector (Vector)
import GHC.Exts (Any)

import qualified Data.Vector as V

import Language.Haskell.TH hiding (cxt)
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
largeRecord opts decls = decls >>= concatMapM go
  where
    go :: Dec -> Q [Dec]
    go (record -> Just r) = process opts r
    go d = fail $ "largeRecord: unexpected declaration " ++ show d

{-------------------------------------------------------------------------------
  Generation
-------------------------------------------------------------------------------}

-- | Generate the newtype that will represent the record
--
-- Generates something like
--
-- > newtype T = TFromVector {vectorFromT :: V.Vector Any}
genNewtype :: Options -> Record -> Q Dec
genNewtype _ r = do
    typVector <- [t| Vector Any |]

    let con :: Con
        con = RecC nameConstr [(
              nameField
            , DefaultBang
            , typVector
            )]

    return $ NewtypeD cxt nameType tyVarBndrs mKind con derivClauses
  where
    nameType, nameConstr, nameField :: Name
    nameType   = recordName       r
    nameConstr = recordConstrName r
    nameField  = recordFieldName  r

    cxt :: Cxt
    cxt = []

    tyVarBndrs :: [TyVarBndr]
    tyVarBndrs = []

    mKind :: Maybe Kind
    mKind = Nothing

    derivClauses :: [DerivClause]
    derivClauses = []

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
      [| $(recordIndexedAccessorQ r) $(fieldIndexQ f) |]

-- | Generate view on the record
--
-- Generates function such as
--
-- > tupleFromT :: T -> (Int, Bool, Char)
-- > tupleFromT = \x -> (tInt x, tBool x, tChar x)
genRecordView :: Options -> Record -> Q [Dec]
genRecordView _opts r@Record{..} = do
    simpleFn
      (recordViewName r)
      [t| $(recordTypeQ r) -> $(return viewType) |]
      viewBody
  where
    viewType :: Type
    viewType = mkTupleT fieldType $ nest recordFields

    viewBody :: Q Exp
    viewBody = do
        x <- newName "x"
        return $ LamE [VarP x] $ mkTupleE (viewField x) $ nest recordFields

    viewField :: Name -> Field -> Exp
    viewField x f = VarE (fieldAccessorName f) `AppE` VarE x

-- | Generate all definitions
process :: Options -> Record -> Q [Dec]
process opts@Options{..} r = concatM $ [
      (:[]) <$> genNewtype opts r
    , genIndexedAccessor opts r
    , genFieldAccessors  opts r
    , when generatePatternSynonym $ [
          genRecordView opts r
        ]
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

recordConstrName :: Record -> Name
recordConstrName Record{..} =
     mkName $ recordUnqual ++ "FromVector"

recordFieldName :: Record -> Name
recordFieldName Record{..} =
    mkName $ "vectorFrom" ++ recordUnqual

recordViewName :: Record -> Name
recordViewName Record{..} =
    mkName $ "tupleFrom" ++ recordUnqual

recordIndexedAccessorName :: Record -> Name
recordIndexedAccessorName Record{..} =
    mkName $ "unsafeIndex" ++ recordUnqual

fieldAccessorName :: Field -> Name
fieldAccessorName Field{..} =
    mkName $ fieldUnqual

{-------------------------------------------------------------------------------
  Derived
-------------------------------------------------------------------------------}

recordTypeQ :: Record -> Q Type
recordTypeQ r = return $ ConT (recordName r)

recordToVectorQ :: Record -> Q Exp
recordToVectorQ r = return $ VarE (recordFieldName r)

recordIndexedAccessorQ :: Record -> Q Exp
recordIndexedAccessorQ r = return $ VarE (recordIndexedAccessorName r)

fieldTypeQ :: Field -> Q Type
fieldTypeQ Field{..} = return fieldType

fieldIndexQ :: Field -> Q Exp
fieldIndexQ Field{..} = return $ LitE . IntegerL $ fromIntegral fieldIndex

{-------------------------------------------------------------------------------
  Supported declarations
-------------------------------------------------------------------------------}

-- | Unqualified name
type Unqual = String

data Record = Record {
      recordUnqual :: Unqual
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
          [RecC _constrName fields]
          _derivClauses@[]
       ) =
        Record (nameBase name)
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
mkTupleT :: forall a. (a -> Type) -> Forest a -> Type
mkTupleT f = forest cata
  where
    cata :: Cata a Type
    cata = Cata {
          leaf   = f
        , branch = \ts -> foldl AppT (TupleT (length ts)) ts
        }

-- | Construct tuple expression
mkTupleE :: forall a. (a -> Exp) -> Forest a -> Exp
mkTupleE f = forest cata
  where
    cata :: Cata a Exp
    cata = Cata {
          leaf   = f
#if MIN_VERSION_template_haskell(2,16,0)
        , branch = TupE . map Just
#else
        , branch = TupE
#endif
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

-- | Example of structure preserving folding
--
-- In Haskell syntax, 'exampleFolded' is equal to @(A, (B, C))@.
_exampleFolded :: Type
_exampleFolded =  mkTupleT id example
  where
    example :: Forest Type
    example =
        Forest [
            Leaf (ConT a)
          , Branch $ Forest [
                Leaf (ConT b)
              , Leaf (ConT c)
              ]
          ]

    a, b, c :: Name
    a = mkName "A"
    b = mkName "B"
    c = mkName "C"

{-------------------------------------------------------------------------------
  Auxiliary: general
-------------------------------------------------------------------------------}

concatM :: Applicative m => [m [a]] -> m [a]
concatM = fmap concat . sequenceA

concatMapM :: Applicative m => (a -> m [b]) -> [a] -> m [b]
concatMapM f = concatM . map f

simpleFn :: Name -> Q Type -> Q Exp -> Q [Dec]
simpleFn name qTyp qBody = do
    typ  <- qTyp
    body <- qBody
    return [
          SigD name typ
        , ValD (VarP name) (NormalB body) []
        ]

chunk :: Int -> [a] -> [[a]]
chunk n = go
  where
    go :: [a] -> [[a]]
    go [] = []
    go xs = let (firstChunk, rest) = splitAt n xs in firstChunk : go rest
