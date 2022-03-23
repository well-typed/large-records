{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DerivingStrategies        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

{-# OPTIONS_GHC -fplugin=RecordDotPreprocessor -fplugin=Data.Record.Plugin #-}

module Test.Record.Beam.Tutorial1 (
    tests

    -- * Exported for the benefit of follow-up tutorials
  , UserT(..)
  , User
  , UserId
  , PrimaryKey(..)
  ) where

import Data.Functor.Identity
import Data.Int
import Data.Kind
import Data.Record.Beam ()
import Data.Text (Text)
import Database.Beam hiding (Generic, countAll_)
import Database.Beam.Schema.Tables

import qualified Data.List.NonEmpty     as NE
import qualified Database.SQLite.Simple as SQLite
import qualified GHC.Generics           as GHC

import Test.Tasty
import Test.Tasty.HUnit

import Test.Record.Beam.Util.Compat
import Test.Record.Beam.Util.SQLite
import Test.Record.Beam.Util.Orphans ()

{-------------------------------------------------------------------------------
  We replicate the beam tutorial, but using large-records

  See <https://haskell-beam.github.io/beam/>
-------------------------------------------------------------------------------}

{-# ANN type UserT largeRecordStrict #-}
data UserT (f :: Type -> Type) = User {
      userEmail     :: Columnar f Text
    , userFirstName :: Columnar f Text
    , userLastName  :: Columnar f Text
    , userPassword  :: Columnar f Text
    }
  deriving stock (Show, Eq)
  deriving anyclass (Beamable)

type User   = UserT Identity
type UserId = PrimaryKey UserT Identity

james, betty, sam :: User
james = User "james@example.com" "James" "Smith"  "b4cc344d25a2efe540adbf2678e2304c"
betty = User "betty@example.com" "Betty" "Jones"  "82b054bd83ffad9b6cf8bdb98ce3cc2f"
sam   = User "sam@example.com"   "Sam"   "Taylor" "332532dcfaa1cbf61e2a266bd723612c"

james2, betty2, james3, sam2, sam3 :: User
james2 = User "james@pallo.com"  "James" "Pallo"   "b4cc344d25a2efe540adbf2678e2304c"
betty2 = User "betty@sims.com"   "Betty" "Sims"    "82b054bd83ffad9b6cf8bdb98ce3cc2f"
james3 = User "james@oreily.com" "James" "O'Reily" "b4cc344d25a2efe540adbf2678e2304c"
sam2   = User "sam@sophitz.com"  "Sam"   "Sophitz" "332532dcfaa1cbf61e2a266bd723612c"
sam3   = User "sam@jely.com"     "Sam"   "Jely"    "332532dcfaa1cbf61e2a266bd723612c"

instance Table UserT where
   data PrimaryKey UserT f = UserId (Columnar f Text)
     deriving stock (GHC.Generic)
     deriving anyclass (Beamable)

   primaryKey tbl = UserId tbl.userEmail

deriving instance Show (Columnar f Text) => Show (PrimaryKey UserT f)
deriving instance Eq   (Columnar f Text) => Eq   (PrimaryKey UserT f)

{-------------------------------------------------------------------------------
  Example DB
-------------------------------------------------------------------------------}

{-# ANN type ShoppingCartDb largeRecordStrict #-}
data ShoppingCartDb (f :: Type -> Type) = ShoppingCartDb {
      shoppingCartUsers :: f (TableEntity UserT)
    }
  deriving (Show, Eq)

instance Database be ShoppingCartDb

shoppingCartDb :: DatabaseSettings be ShoppingCartDb
shoppingCartDb = defaultDbSettings

{-------------------------------------------------------------------------------
  Tests proper
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.Record.Beam.Tutorial1" [
      testCase "tblSkeleton"       test_tutorial1_tblSkeleton
    , testCase "defaultDbSettings" test_tutorial1_defaultDbSettings
    , testCase "insertSelect"      test_tutorial1_insertSelect
    , testCase "recordDotSyntax"   test_tutorial1_recordDotSyntax
    ]

test_tutorial1_tblSkeleton :: Assertion
test_tutorial1_tblSkeleton =
    assertEqual "" expected (tblSkeleton :: TableSkeleton UserT)
  where
    expected :: TableSkeleton UserT
    expected = User {
            userEmail     = Ignored
          , userFirstName = Ignored
          , userLastName  = Ignored
          , userPassword  = Ignored
        }

test_tutorial1_defaultDbSettings :: Assertion
test_tutorial1_defaultDbSettings =
    assertEqual "" expected shoppingCartDb
  where
    expected :: DatabaseSettings be ShoppingCartDb
    expected = ShoppingCartDb {
            shoppingCartUsers = DatabaseEntity (
              DatabaseTable {
                dbTableSchema      = Nothing
              , dbTableOrigName    = "shoppingCartUsers"
              , dbTableCurrentName = "cart_users"
              , dbTableSettings    = User {
                      userEmail     = TableField { _fieldPath = NE.fromList ["userEmail"]     , _fieldName = "email"}
                    , userFirstName = TableField { _fieldPath = NE.fromList ["userFirstName"] , _fieldName = "first_name"}
                    , userLastName  = TableField { _fieldPath = NE.fromList ["userLastName"]  , _fieldName = "last_name"}
                    , userPassword  = TableField { _fieldPath = NE.fromList ["userPassword"]  , _fieldName = "password"}
                  }
              }
          )
        }

test_tutorial1_insertSelect :: Assertion
test_tutorial1_insertSelect = runInMemory $ \conn -> do
    liftIO $ SQLite.execute_ conn $
      "CREATE TABLE cart_users (email VARCHAR NOT NULL, first_name VARCHAR NOT NULL, last_name VARCHAR NOT NULL, password VARCHAR NOT NULL, PRIMARY KEY( email ));"

    runInsert $ insert shoppingCartDb.shoppingCartUsers $ insertValues [
         james
       , betty
       , sam
       ]

    let allUsers = all_ (shoppingCartDb.shoppingCartUsers)
    users <- runSelectReturningList $ select allUsers
    liftIO $ assertEqual "users" [james, betty, sam] users

    let sortUsersByFirstName = orderBy_ (\u -> (asc_ u.userFirstName, desc_ u.userLastName)) (all_ shoppingCartDb.shoppingCartUsers)
    sorted <- runSelectReturningList $ select sortUsersByFirstName
    liftIO $ assertEqual "sorted" [betty, james, sam] sorted

    let boundedQuery = limit_ 1 $ offset_ 1 $
                       orderBy_ (\u -> asc_ u.userFirstName) $
                       all_ shoppingCartDb.shoppingCartUsers

    bounded <- runSelectReturningList (select boundedQuery)
    liftIO $ assertEqual "bounded" [james] bounded

    -- Tutorial has Int32 here, but that doesn't typecheck
    -- Don't think that is related to beam-large-records though..?
    -- (Maybe due to beam version mismatch between tutorial and our beam branch.)
    let userCount = aggregate_ (\_u -> as_ @Int32 countAll_) (all_ shoppingCartDb.shoppingCartUsers)
    Just c <- runSelectReturningOne $ select userCount
    liftIO $ assertEqual "userCount" 3 c

    runInsert $ insert shoppingCartDb.shoppingCartUsers $ insertValues [
        james2
      , betty2
      , james3
      , sam2
      , sam3
      ]
    let numberOfUsersByName = aggregate_ (\u -> (group_ u.userFirstName, as_ @Int32 countAll_)) $
                              all_ shoppingCartDb.shoppingCartUsers
    countedByName <- runSelectReturningList $ select numberOfUsersByName
    liftIO $ assertEqual "countedByName" [("Betty",2), ("James",3), ("Sam",3)] countedByName

-- Just a sanity check that RDS is working
-- (NOTE: RDS gets confused by nested multiline comments.)
test_tutorial1_recordDotSyntax :: Assertion
test_tutorial1_recordDotSyntax =
    assertEqual "" "a@b.c" u.userEmail
  where
    u :: User
    u = User {
          userEmail     = "a@b.c"
        , userFirstName = "John"
        , userLastName  = "Doe"
        , userPassword  = "secret"
        }

