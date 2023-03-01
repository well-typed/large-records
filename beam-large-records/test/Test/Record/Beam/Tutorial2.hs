{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DerivingStrategies        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}

{-# OPTIONS_GHC -fplugin=Data.Record.Plugin.WithRDP #-}

{-# OPTIONS_GHC -Wno-missing-signatures -Wno-unused-top-binds #-}

module Test.Record.Beam.Tutorial2 (
    tests

    -- * Exported for the benefit of follow-up tutorials
  , AddressT(..)
  , Address
  , PrimaryKey(..)
  ) where

import Data.Functor.Const
import Data.Int
import Data.Kind
import Data.Record.Plugin
import Data.Text (Text)
import Database.Beam
import Database.Beam.Schema.Tables
import Lens.Micro

import qualified Data.List.NonEmpty     as NE
import qualified Data.Text              as Text
import qualified Database.SQLite.Simple as SQLite
import qualified GHC.Generics           as GHC

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, assertEqual)

import Test.Record.Beam.Tutorial1 hiding (tests)
import Test.Record.Beam.Util.SQLite

{-------------------------------------------------------------------------------
  New table: with a foreign key
-------------------------------------------------------------------------------}

{-# ANN type AddressT largeRecord #-}
data AddressT (f :: Type -> Type) = Address {
      addressId      :: C f Int32
    , addressLine1   :: C f Text
    , addressLine2   :: C f (Maybe Text)
    , addressCity    :: C f Text
    , addressState   :: C f Text
    , addressZip     :: C f Text
    , addressForUser :: PrimaryKey UserT f
    }
  deriving (Show, Eq)
  deriving anyclass (Beamable)

type Address   = AddressT Identity
-- type AddressId = PrimaryKey AddressT Identity

instance Table AddressT where
  data PrimaryKey AddressT f = AddressId (Columnar f Int32)
    deriving stock (GHC.Generic)
    deriving anyclass (Beamable)

  primaryKey addr = AddressId $ addr.addressId

deriving instance Show (Columnar f Int32) => Show (PrimaryKey AddressT f)
deriving instance Eq   (Columnar f Int32) => Eq   (PrimaryKey AddressT f)

exampleAddress :: Address
exampleAddress = Address {
      addressId      = 1
    , addressLine1   = "street"
    , addressLine2   = Nothing
    , addressCity    = "city"
    , addressState   = "state"
    , addressZip     = "zip"
    , addressForUser = UserId "a@b.c"
    }

{-------------------------------------------------------------------------------
  Version 2 of the DB
-------------------------------------------------------------------------------}

{-# ANN type ShoppingCart2Db largeRecord #-}
data ShoppingCart2Db (f :: Type -> Type) = ShoppingCart2Db {
     shoppingCart2Users         :: f (TableEntity UserT)
   , shoppingCart2UserAddresses :: f (TableEntity AddressT)
   }
 deriving (Show, Eq)

instance Database be ShoppingCart2Db

shoppingCart2Db :: forall be. DatabaseSettings be ShoppingCart2Db
shoppingCart2Db = defaultDbSettings `withDbModification`
    dbModification{shoppingCart2UserAddresses =
             setEntityName "addresses"
          <> modifyTableFields
               tableModification{addressLine1 = fieldNamed "address1"
                                ,addressLine2 = fieldNamed "address2"
                                }
      }

{-------------------------------------------------------------------------------
  Derive lenses

  TODO: Can we avoid the type signature on 'lensesAddressT' and co?
-------------------------------------------------------------------------------}

lensesAddressT :: AddressT (Lenses AddressT f)
lensesUserT    :: UserT    (Lenses UserT    f)

lensesAddressT = tableLenses
lensesUserT    = tableLenses

lensesShoppingCart2 :: ShoppingCart2Db (TableLens f ShoppingCart2Db)
lensesShoppingCart2 = dbLenses

xaddressId        :: Lens' (AddressT f) (Columnar f Int32)
xaddressId        = case lensesAddressT.addressId      of LensFor x -> x

xaddressLine1     :: Lens' (AddressT f) (Columnar f Text)
xaddressLine1     = case lensesAddressT.addressLine1   of LensFor x -> x

xaddressLine2     :: Lens' (AddressT f) (Columnar f (Maybe Text))
xaddressLine2     = case lensesAddressT.addressLine2   of LensFor x -> x

xaddressCity      :: Lens' (AddressT f) (Columnar f Text)
xaddressCity      = case lensesAddressT.addressCity    of LensFor x -> x

xaddressState     :: Lens' (AddressT f) (Columnar f Text)
xaddressState     = case lensesAddressT.addressState   of LensFor x -> x

xaddressZip       :: Lens' (AddressT f) (Columnar f Text)
xaddressZip       = case lensesAddressT.addressZip     of LensFor x -> x

xaddressForUserId :: Lens' (AddressT f) (Columnar f Text)
xaddressForUserId = case lensesAddressT.addressForUser of UserId (LensFor x) -> x


xuserEmail     :: Lens' (UserT f) (Columnar f Text)
xuserEmail     = case lensesUserT.userEmail of LensFor x -> x

xuserFirstName :: Lens' (UserT f) (Columnar f Text)
xuserFirstName = case lensesUserT.userFirstName of LensFor x -> x

xuserLastName  :: Lens' (UserT f) (Columnar f Text)
xuserLastName  = case lensesUserT.userLastName of LensFor x -> x

xuserPassword  :: Lens' (UserT f) (Columnar f Text)
xuserPassword  = case lensesUserT.userPassword of LensFor x -> x


xshoppingCart2Users         :: Lens' (ShoppingCart2Db f) (f (TableEntity UserT))
xshoppingCart2Users         = case lensesShoppingCart2.shoppingCart2Users of TableLens x -> x

xshoppingCart2UserAddresses :: Lens' (ShoppingCart2Db f) (f (TableEntity AddressT))
xshoppingCart2UserAddresses = case lensesShoppingCart2.shoppingCart2UserAddresses of TableLens x -> x

{-------------------------------------------------------------------------------
  Tests proper
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.Record.Beam.Tutorial2" [
      testCase "defaultDbSettings" test_tutorial2_defaultDbSettings
    , testCase "tableLenses"       test_tableLenses
    , testCase "dbLenses"          test_dbLenses
    , testCase "SQL"               test_SQL
    ]

test_tutorial2_defaultDbSettings :: Assertion
test_tutorial2_defaultDbSettings =
    assertEqual "" expected shoppingCart2Db
  where
    expected :: DatabaseSettings be ShoppingCart2Db
    expected = ShoppingCart2Db {
            shoppingCart2Users = DatabaseEntity $ DatabaseTable {
                dbTableSchema      = Nothing
              , dbTableOrigName    = "shoppingCart2Users"
              , dbTableCurrentName = "cart2_users"
              , dbTableSettings    = User {
                      userEmail     = TableField {_fieldPath = NE.fromList ["userEmail"]     , _fieldName = "email"}
                    , userFirstName = TableField {_fieldPath = NE.fromList ["userFirstName"] , _fieldName = "first_name"}
                    , userLastName  = TableField {_fieldPath = NE.fromList ["userLastName"]  , _fieldName = "last_name"}
                    , userPassword  = TableField {_fieldPath = NE.fromList ["userPassword"]  , _fieldName = "password"}
                  }
              }
          , shoppingCart2UserAddresses = DatabaseEntity $ DatabaseTable {
                dbTableSchema      = Nothing
              , dbTableOrigName    = "shoppingCart2UserAddresses"
              , dbTableCurrentName = "addresses"
              , dbTableSettings    = Address {
                    addressId      = TableField {_fieldPath = NE.fromList ["addressId"]    , _fieldName = "id"}
                  , addressLine1   = TableField {_fieldPath = NE.fromList ["addressLine1"] , _fieldName = "address1"}
                  , addressLine2   = TableField {_fieldPath = NE.fromList ["addressLine2"] , _fieldName = "address2"}
                  , addressCity    = TableField {_fieldPath = NE.fromList ["addressCity"]  , _fieldName = "city"}
                  , addressState   = TableField {_fieldPath = NE.fromList ["addressState"] , _fieldName = "state"}
                  , addressZip     = TableField {_fieldPath = NE.fromList ["addressZip"]   , _fieldName = "zip"}
                  , addressForUser = UserId $ TableField {
                        _fieldPath = NE.fromList ["addressForUser", "userEmail"]
                      , _fieldName = "for_user__email"
                      }

                  }
              }
        }

test_tableLenses :: Assertion
test_tableLenses = do
    assertEqual "get" expectedGet $
      exampleAddress ^. xaddressId
    assertEqual "set" expectedSet $
      exampleAddress & xaddressForUserId %~ Text.toUpper
  where
    expectedGet :: Int32
    expectedGet = 1

    expectedSet :: Address
    expectedSet = exampleAddress{addressForUser = UserId "A@B.C"}

test_dbLenses :: Assertion
test_dbLenses = do
    assertEqual "get" expectedGet $
      exampleDb ^. xshoppingCart2Users
    assertEqual "set" expectedSet $
      exampleDb & xshoppingCart2UserAddresses %~ (\(Const n) -> Const (n + 1))
  where
    expectedGet :: Const Int a
    expectedGet = Const 1

    exampleDb, expectedSet :: ShoppingCart2Db (Const Int)
    exampleDb = ShoppingCart2Db {
          shoppingCart2Users         = Const 1
        , shoppingCart2UserAddresses = Const 2
        }
    expectedSet = exampleDb{shoppingCart2UserAddresses = Const 3}

test_SQL :: Assertion
test_SQL = runInMemory $ \conn -> do
    liftIO $ SQLite.execute_ conn $
      "CREATE TABLE cart2_users (email VARCHAR NOT NULL, first_name VARCHAR NOT NULL, last_name VARCHAR NOT NULL, password VARCHAR NOT NULL, PRIMARY KEY( email ));"
    liftIO $ SQLite.execute_ conn $
      "CREATE TABLE addresses ( id INTEGER PRIMARY KEY, address1 VARCHAR NOT NULL, address2 VARCHAR, city VARCHAR NOT NULL, state VARCHAR NOT NULL, zip VARCHAR NOT NULL, for_user__email VARCHAR NOT NULL );"

    runInsert $ insert shoppingCart2Db.shoppingCart2Users $
      insertValues [ james, betty, sam ]
    runInsert $ insert shoppingCart2Db.shoppingCart2UserAddresses $
      insertExpressions addresses

    -- Straight-forward SELECT
    -- (Checks that primary keys have been assigned correctly)
    addressesActual <-
      runSelectReturningList $
        select (all_ (shoppingCart2Db ^. xshoppingCart2UserAddresses))
    liftIO $ assertEqual "addresses"
               addressesExpected
               addressesActual

    -- Simple JOIN
    usersAndRelatedAddressesActual <-
      runSelectReturningList $ select $ do
        user    <- all_ (shoppingCart2Db ^. xshoppingCart2Users)
        address <- all_ (shoppingCart2Db ^. xshoppingCart2UserAddresses)
        guard_ (address ^. xaddressForUserId ==. user ^. xuserEmail)
        return (user, address)
    liftIO $ assertEqual "usersAndRelatedAddresses"
               usersAndRelatedAddressesExpected
               usersAndRelatedAddressesActual

    -- Alternative way to write the same JOIN
    usersAndRelatedAddressesUsingReferences <-
      runSelectReturningList $ select $ do
        user    <- all_ (shoppingCart2Db ^. xshoppingCart2Users)
        address <- all_ (shoppingCart2Db ^. xshoppingCart2UserAddresses)
        guard_ (address.addressForUser `references_` user)
        pure (user, address)
    liftIO $ assertEqual "usersAndRelatedAddressesUsingReferences"
               usersAndRelatedAddressesExpected
               usersAndRelatedAddressesUsingReferences

    -- Using ON
    usersAndRelatedAddressesUsingRelated <-
      runSelectReturningList $ select $ do
        address <- all_     (shoppingCart2Db ^. xshoppingCart2UserAddresses)
        user    <- related_ (shoppingCart2Db ^. xshoppingCart2Users) address.addressForUser
        pure (user, address)
    liftIO $ assertEqual "usersAndRelatedAddressesUsingRelated"
               usersAndRelatedAddressesExpected
               usersAndRelatedAddressesUsingRelated

    -- WHERE on a foreign key
    bettysAddresses <-
      runSelectReturningList $ select $ do
        address <- all_ (shoppingCart2Db ^. xshoppingCart2UserAddresses)
        guard_ (address.addressForUser ==. val_ bettyId)
        pure address
    liftIO $ assertEqual "bettysAddresses"
               [addr2, addr3]
               bettysAddresses

    -- Simple UPDATE
    runUpdate $ save (shoppingCart2Db ^. xshoppingCart2Users) $
      james{userPassword = superSecure}
    [james'] <- runSelectReturningList $
      lookup_ (shoppingCart2Db ^. xshoppingCart2Users) jamesId
    liftIO $ assertEqual "James' new password"
               superSecure
               (james' ^. xuserPassword)

    -- More granular UPDATE
    runUpdate $ update (shoppingCart2Db ^. xshoppingCart2UserAddresses)
        (\address -> mconcat [
              address ^. xaddressCity <-. val_ "Sugarville"
            , address ^. xaddressZip  <-. val_ "12345"
            ]
        )
        (\address ->
                address ^. xaddressCity  ==. val_ "Sugarland"
            &&. address ^. xaddressState ==. val_ "TX"
        )
    updatedAddresses <- runSelectReturningList $
      select $ all_ (shoppingCart2Db ^. xshoppingCart2UserAddresses)
    liftIO $ assertEqual "updatedAddresses"
               [addr1, addr2, addr3']
               updatedAddresses

    -- DELETE
    runDelete $ delete (shoppingCart2Db ^. xshoppingCart2UserAddresses)
      (\address ->
              address ^. xaddressCity ==. "Houston"
          &&. address.addressForUser `references_` val_ betty
      )
    afterDelete <- runSelectReturningList $
      select $ all_ (shoppingCart2Db ^. xshoppingCart2UserAddresses)
    liftIO $ assertEqual "afterDelete"
               [addr1, addr3']
               afterDelete
  where
    james, betty, sam :: User
    james = User "james@example.com" "James" "Smith"  "b4cc344d25a2efe540adbf2678e2304c"
    betty = User "betty@example.com" "Betty" "Jones"  "82b054bd83ffad9b6cf8bdb98ce3cc2f"
    sam   = User "sam@example.com"   "Sam"   "Taylor" "332532dcfaa1cbf61e2a266bd723612c"

    jamesId, bettyId :: UserId
    jamesId = UserId "james@example.com"
    bettyId = UserId "betty@example.com"

    -- The tutorial uses @pk@ directly, rather than @val . pk@.
    -- This is possible if we make @james@ and co polymorphic
    --
    -- > james :: UserT (QExpr Sqlite s)
    --
    -- We can do that (because of a 'IsString' instance for 'QExpr', but then we
    -- get into trouble in @addr1@ and co.
    addresses :: [AddressT (QExpr Sqlite s)]
    addresses = [
          Address default_ (val_ "123 Little Street")  (val_ Nothing)        (val_ "Boston")    (val_ "MA") (val_ "12345") (val_ (pk james))
        , Address default_ (val_ "222 Main Street")    (val_ (Just "Ste 1")) (val_ "Houston")   (val_ "TX") (val_ "8888")  (val_ (pk betty))
        , Address default_ (val_ "9999 Residence Ave") (val_ Nothing)        (val_ "Sugarland") (val_ "TX") (val_ "8989")  (val_ (pk betty))
        ]

    addr1, addr2, addr3, addr3' :: Address
    addr1  = Address 1 "123 Little Street"  Nothing        "Boston"     "MA" "12345" (pk james)
    addr2  = Address 2 "222 Main Street"    (Just "Ste 1") "Houston"    "TX" "8888"  (pk betty)
    addr3  = Address 3 "9999 Residence Ave" Nothing        "Sugarland"  "TX" "8989"  (pk betty)
    addr3' = Address 3 "9999 Residence Ave" Nothing        "Sugarville" "TX" "12345" (pk betty)

    addressesExpected :: [Address]
    addressesExpected = [
          addr1
        , addr2
        , addr3
        ]

    usersAndRelatedAddressesExpected :: [(User, Address)]
    usersAndRelatedAddressesExpected = [
          (james, addr1)
        , (betty, addr2)
        , (betty, addr3)
        ]

    superSecure :: Text
    superSecure = "52a516ca6df436828d9c0d26e31ef704"

