{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE EmptyCase           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -fplugin=Data.Record.Anon.Plugin #-}

module Test.Prop.Record.Diff (tests) where

import Data.Kind
import Data.Map (Map)
import Data.SOP
import Data.Type.Equality
import GHC.TypeLits
import Test.QuickCheck
import Test.Tasty hiding (after)
import Test.Tasty.QuickCheck

import qualified Data.Map as Map

import Data.Record.Anon
import Data.Record.Anon.Advanced (Record)
import qualified Data.Record.Anon.Advanced as Anon

{-------------------------------------------------------------------------------
  Abstraction over the 'Diff' operations
-------------------------------------------------------------------------------}

data Op :: Row Type -> Row Type -> Type where
  -- | Insert new field into the record
  Insert :: KnownSymbol n => Field n -> Int -> Op r (n := Int : r)

  -- | Get field from the record and check that it has the correct value
  --
  -- (The test itself will have to keep track of what the correct value /is/)
  Get :: (KnownSymbol n, RowHasField n r Int) => Field n -> Op r r

  -- | Update value of a field in the record
  Set :: (KnownSymbol n, RowHasField n r Int) => Field n -> Int -> Op r r

  -- | Apply all pending changes
  Apply  :: Op r r

deriving instance Show (Op r r')

data Ops :: Row Type -> Row Type -> Type where
  Done :: Ops r r
  Op   :: IsKnownRow r' => Op r r' -> Ops r' r'' -> Ops r r''


data TestResult = TestOk | TestFailed String
  deriving (Show, Eq)

-- | Model to test the actual record against
type RecordModel = Map String Int

execute :: Ops '[] r -> TestResult
execute = go Anon.empty Map.empty
  where
    go :: Record I r -> RecordModel -> Ops r r' -> TestResult
    go _ _ Done      = TestOk
    go r m (Op o os) =
        case o of
          Insert f v ->
            go (Anon.insert f (I v) r) (Map.insert (symbolVal f) v m) os
          Get f ->
            case Map.lookup (symbolVal f) m of
              Nothing ->
                TestFailed "Unknown field (this indicates a bug in the tests"
              Just expected ->
                let actual = unI (Anon.get f r) in
                if actual == expected
                  then go r m os
                  else TestFailed $ concat [
                      "Expected " ++ show expected
                    , ", got " ++ show actual
                    ]
          Set f v ->
            go (Anon.set f (I v) r) (Map.insert (symbolVal f) v m) os
          Apply ->
            go (Anon.applyPending r) m os

{-------------------------------------------------------------------------------
  Existential wrappers
-------------------------------------------------------------------------------}

data SomeField r where
  SomeField :: (KnownSymbol n, RowHasField n r Int) => Field n -> SomeField r

data SomeOp r where
  SomeOp :: forall r r'. IsKnownRow r' => Op r r' -> SomeOp r

data SomeOps r where
  SomeOps :: forall r r'. IsKnownRow r' => Ops r r' -> SomeOps r

{-------------------------------------------------------------------------------
  Show instances

  These are not lawful (they are not valid Haskell), but try to present
  readable counter-examples.
-------------------------------------------------------------------------------}

-- | Used only for 'Show'
data UnknownOp where
  UnknownOp :: Op r r' -> UnknownOp

opsToList :: Ops r r' -> [UnknownOp]
opsToList Done        = []
opsToList (Op op ops) = UnknownOp op : opsToList ops

instance Show UnknownOp     where show (UnknownOp x) = show x
instance Show (SomeField r) where show (SomeField x) = show x
instance Show (SomeOp    r) where show (SomeOp    x) = show x
instance Show (SomeOps   r) where show (SomeOps   x) = show (opsToList x)

{-------------------------------------------------------------------------------
  Generation
-------------------------------------------------------------------------------}

-- | Known row
--
-- @large-anon@ is not designed for inductive reasoning, which makes
-- constructing a random record iteratively rather difficult. We could use the
-- (unsafe) low level API, but part of what we want to test here is the high
-- level translation the library does in the plugin. We therefore restrict our
-- attention to specific valid rows.
--
-- TODO: If we add allow for permutations, we can also test 'Anon.project'.
data KnownRow :: Row Type -> Type where
  Valid0 :: KnownRow '[]
  Valid1 :: KnownRow '[ "f1" := Int ]
  Valid2 :: KnownRow '[ "f2" := Int, "f1" := Int ]
  Valid3 :: KnownRow '[ "f3" := Int, "f2" := Int, "f1" := Int ]

class IsKnownRow (row :: Row Type) where
  isKnownRow :: proxy row -> KnownRow row

instance IsKnownRow '[]                                        where isKnownRow _ = Valid0
instance IsKnownRow '[ "f1" := Int ]                           where isKnownRow _ = Valid1
instance IsKnownRow '[ "f2" := Int, "f1" := Int ]              where isKnownRow _ = Valid2
instance IsKnownRow '[ "f3" := Int, "f2" := Int, "f1" := Int ] where isKnownRow _ = Valid3



tryPickExisting :: KnownRow r -> Maybe (Gen (SomeField r))
tryPickExisting Valid0 = Nothing
tryPickExisting Valid1 = Just $ elements [ SomeField #f1 ]
tryPickExisting Valid2 = Just $ elements [ SomeField #f1
                                         , SomeField #f2
                                         ]
tryPickExisting Valid3 = Just $ elements [ SomeField #f1
                                         , SomeField #f2
                                         , SomeField #f3
                                         ]

tryGenInsert :: KnownRow r -> Maybe (Int -> Gen (SomeOp r))
tryGenInsert Valid0 = Just $ \v -> return $ SomeOp (Insert #f1 v)
tryGenInsert Valid1 = Just $ \v -> return $ SomeOp (Insert #f2 v)
tryGenInsert Valid2 = Just $ \v -> return $ SomeOp (Insert #f3 v)
tryGenInsert Valid3 = Nothing

genOp :: forall proxy r. IsKnownRow r => proxy r -> Gen (SomeOp r)
genOp r = oneof . concat $ [
      [ do SomeField field <- genField
           return $ SomeOp (Get field)
      | Just genField <- [tryPickExisting $ isKnownRow r]
      ]
    , [ do SomeField field <- genField
           newValue <- choose (0, 100)
           return $ SomeOp (Set field newValue)
      | Just genField <- [tryPickExisting $ isKnownRow r]
      ]
    , [ do v <- choose (0, 100)
           genInsert v
      | Just genInsert <- [tryGenInsert (isKnownRow r)]
      ]
    , [ return $ SomeOp Apply ]
    ]

genOpsFrom :: IsKnownRow r => proxy r -> Int -> Gen (SomeOps r)
genOpsFrom _ 0 = return $ SomeOps Done
genOpsFrom r n = do SomeOp  op  <- genOp      r
                    SomeOps ops <- genOpsFrom op (pred n)
                    return $ SomeOps (Op op ops)

genOps :: Gen (SomeOps '[])
genOps = do
    n <- choose (0, 10)
    genOpsFrom Anon.empty n

{-------------------------------------------------------------------------------
  Shrinking

  We could be much more sophisticated in how we shrink, but now for we just pick
  the low hanging fruit only. Specifically, we do not try to omit/change any
  instructions that affect types (specifically, inserts).
-------------------------------------------------------------------------------}

sameResultRow :: Op r r' -> Maybe (r :~: r')
sameResultRow (Insert _ _) = Nothing
sameResultRow (Get _)      = Just Refl
sameResultRow (Set _ _)    = Just Refl
sameResultRow Apply        = Just Refl

isDone :: Ops r r' -> Bool
isDone Done     = True
isDone (Op _ _) = False

shrinkOp :: Op r r' -> [Op r r']
shrinkOp (Insert f n) = Insert f <$> shrink n
shrinkOp (Get _)      = []
shrinkOp (Set f n)    = Set f <$> shrink n
shrinkOp Apply        = []

shrinkOps :: IsKnownRow r' => Ops r r' -> [SomeOps r]
shrinkOps Done        = []
shrinkOps (Op op ops) = concat [
      -- Shrink the operation
      [ SomeOps $ Op op' ops
      | op' <- shrinkOp op
      ]

      -- Shrink the tail
    , [ SomeOps $ Op op ops'
      | SomeOps ops' <- shrinkOps ops
      ]

      -- Drop the operation
    , [ SomeOps $ ops
      | Just Refl <- [sameResultRow op]
      ]

      -- Drop the tail
    , [ SomeOps $ Op op Done
      | not $ isDone ops
      ]
    ]

{-------------------------------------------------------------------------------
  Arbitrary instance
-------------------------------------------------------------------------------}

instance Arbitrary (SomeOps '[]) where
  arbitrary            = genOps
  shrink (SomeOps ops) = shrinkOps ops

{-------------------------------------------------------------------------------
  Top-level

  These tests test the various ways to construct a record bit by bit, thereby
  constructing a 'Diff' (and occassionally applying that 'Diff').
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.Prop.Record.Diff" [
      testProperty "diff" test_diff
    ]

test_diff :: SomeOps '[] -> Property
test_diff (SomeOps ops) = execute ops === TestOk
