{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -fprint-explicit-kinds #-}
{-# OPTIONS_GHC -Wno-orphans           #-}

module Test.Record.Prop.RedBlackTree (tests) where

import GHC.TypeLits

-- TODO: Think about strictness
import qualified Data.Map.Lazy                      as Model
import qualified Data.Record.Anonymous.RedBlackTree as LR (Map)
import qualified Data.Record.Anonymous.RedBlackTree as LR.Map

import Test.Tasty
import Test.Tasty.QuickCheck

import Data.Record.Anonymous.Util.Singleton
import Data.Record.Anonymous.Util.Promotion

tests :: TestTree
tests = testGroup "Test.Record.Prop.RedBlackTree" [
      testProperty "conversion" prop_conversion
    , testGroup "term-level" [
          testProperty "insert" prop_term_insert
        , testProperty "lookup" prop_term_lookup
        ]
    , testGroup "type-level" [
          testProperty "insert" prop_type_insert
        , testProperty "lookup" prop_type_lookup
        ]
    ]

{-------------------------------------------------------------------------------
  We use Map from containers as a model
-------------------------------------------------------------------------------}

newtype ModelMap = ModelMap (Model.Map Key Val)
  deriving (Eq)

newtype Key = Key { getKey :: String  } deriving newtype (Eq, Ord, Show)
newtype Val = Val { getVal :: Integer } deriving newtype (Eq, Ord, Show)

modelToList :: ModelMap -> [(Key, Val)]
modelToList (ModelMap m) = Model.toList m

modelFromList :: [(Key, Val)] -> ModelMap
modelFromList m = ModelMap (Model.fromList m)

instance Show ModelMap where
  show = show . modelToList

instance Arbitrary Key where
  arbitrary = Key . (:[]) <$> elements ['a' .. 'e']
  shrink (Key [c]) = [Key [c'] | c' <- ['a' .. pred c]]
  shrink (Key _) = error "shrink: unexpected key"

instance Arbitrary Val where
  arbitrary = (\(NonNegative (Small n)) -> Val n) <$> arbitrary
  shrink (Val n) = Val <$> shrink n

instance Arbitrary ModelMap where
  arbitrary = modelFromList <$> arbitrary
  shrink    = fmap modelFromList . shrink . modelToList

{-------------------------------------------------------------------------------
  Conversion from and to the model (term-level)
-------------------------------------------------------------------------------}

fromModel :: Ord k => Model.Map k a -> LR.Map k a
fromModel = LR.Map.fromList . Model.toList

toModel :: Ord k => LR.Map k a -> Model.Map k a
toModel = Model.fromList . LR.Map.toList

{-------------------------------------------------------------------------------
  Conversion from and to the model (type-level)
-------------------------------------------------------------------------------}

type instance Promoted Key = Symbol
type instance Promoted Val = Nat
type instance Promoted (Model.Map k a) = LR.Map (Promoted k) (Promoted a)

instance Bounce Key where
  promote = promote . getKey
  demote  = Key . demote

instance Bounce Val where
  promote = promote . getVal
  demote  = Val . demote

-- | Bounce the model map
--
-- We define this in terms of 'fromModel' and 'toModel'; this is justified by
-- 'prop_conversion'.
instance (Ord k, Bounce k, Bounce a) => Bounce (Model.Map k a) where
  promote = promote . fromModel
  demote  = toModel . demote

{-------------------------------------------------------------------------------
  Conversion
-------------------------------------------------------------------------------}

prop_conversion :: ModelMap -> Property
prop_conversion (ModelMap m) =
        m
    === toModel (fromModel m)

{-------------------------------------------------------------------------------
  Term-level
-------------------------------------------------------------------------------}

prop_term_insert :: ModelMap -> Key -> Val -> Property
prop_term_insert (ModelMap m) k v =
        Model.insert k v m
    === toModel (LR.Map.insert k v (fromModel m))

prop_term_lookup :: ModelMap -> Key -> Property
prop_term_lookup (ModelMap m) k =
        Model.lookup k m
    === LR.Map.lookup k (fromModel m)

{-------------------------------------------------------------------------------
  Type-level
-------------------------------------------------------------------------------}

prop_type_insert :: ModelMap -> Key -> Val -> Property
prop_type_insert (ModelMap m) k v =
    case (promote m, promote k, promote v) of
      (SomeSing m', SomeSing k', SomeSing v') ->
             Model.insert k v m
         === demote (LR.Map.singInsert k' v' m')

prop_type_lookup :: ModelMap -> Key -> Property
prop_type_lookup (ModelMap m) k =
    case (promote m, promote k) of
      (SomeSing m', SomeSing k') ->
             Model.lookup k m
         === demote (LR.Map.singLookup k' m')