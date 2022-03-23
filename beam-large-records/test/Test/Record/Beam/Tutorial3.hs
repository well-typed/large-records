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

-- For lens derivation
{-# LANGUAGE ImpredicativeTypes #-}
{-# OPTIONS_GHC -Wno-missing-signatures -Wno-unused-top-binds #-}

module Test.Record.Beam.Tutorial3 (tests) where

import Prelude hiding (product)

import Data.Int
import Data.Kind
import Data.Text (Text)
import Data.Time
import Database.Beam hiding (countAll_)
import Database.Beam.Backend.SQL
import Database.Beam.Sqlite
import Lens.Micro

import qualified Data.Text              as T
import qualified Database.SQLite.Simple as SQLite
import qualified GHC.Generics           as GHC

import Test.Tasty
import Test.Tasty.HUnit

import Data.Record.Beam ()

import Test.Record.Beam.Tutorial1 hiding (tests)
import Test.Record.Beam.Tutorial2 hiding (tests)
import Test.Record.Beam.Util.Compat
import Test.Record.Beam.Util.SQLite

{-------------------------------------------------------------------------------
  New table: Product

  This does not introduce a new beam features.
-------------------------------------------------------------------------------}

{-# ANN type ProductT largeRecordStrict #-}
data ProductT (f :: Type -> Type) = Product {
     productId          :: C f Int32
   , productTitle       :: C f Text
   , productDescription :: C f Text
   , productPrice       :: C f Int32 {- Price in cents -}
   }
 deriving (Show, Eq)
 deriving anyclass (Beamable)

instance Table ProductT where
  data PrimaryKey ProductT f = ProductId (Columnar f Int32)
    deriving stock (GHC.Generic)
    deriving anyclass (Beamable)

  primaryKey p = ProductId p.productId

deriving instance Show (Columnar f Int32) => Show (PrimaryKey ProductT f)
deriving instance Eq   (Columnar f Int32) => Eq   (PrimaryKey ProductT f)

type Product = ProductT Identity

{-------------------------------------------------------------------------------
  New table: Order

  This introduces the use of 'Nullable' (as well as the use of a
  custom datatype for a column).
-------------------------------------------------------------------------------}

data ShippingCarrier = USPS | FedEx | UPS | DHL
  deriving (Show, Read, Eq, Ord, Enum)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be ShippingCarrier where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Sqlite ShippingCarrier where
  fromBackendRow = read . T.unpack <$> fromBackendRow

{-# ANN type OrderT largeRecordStrict #-}
data OrderT (f :: Type -> Type) = Order {
      orderId            :: Columnar f Int32
    , orderDate          :: Columnar f LocalTime
    , orderForUser       :: PrimaryKey UserT f
    , orderShipToAddress :: PrimaryKey AddressT f
    , orderShippingInfo  :: PrimaryKey ShippingInfoT (Nullable f)
    }
  deriving (Show, Eq)
  deriving anyclass (Beamable)

{-# ANN type ShippingInfoT largeRecordStrict #-}
data ShippingInfoT (f :: Type -> Type) = ShippingInfo {
      shippingInfoId             :: Columnar f Int32
    , shippingInfoCarrier        :: Columnar f ShippingCarrier
    , shippingInfoTrackingNumber :: Columnar f Text
    }
  deriving (Show, Eq)
  deriving anyclass (Beamable)

instance Table OrderT where
  data PrimaryKey OrderT f = OrderId (Columnar f Int32)
    deriving stock (GHC.Generic)
    deriving anyclass (Beamable)

  primaryKey o = OrderId o.orderId

instance Table ShippingInfoT where
  data PrimaryKey ShippingInfoT f = ShippingInfoId (Columnar f Int32)
    deriving stock (GHC.Generic)
    deriving anyclass (Beamable)

  primaryKey s = ShippingInfoId s.shippingInfoId

deriving instance Show (Columnar f Int32) => Show (PrimaryKey OrderT f)
deriving instance Eq   (Columnar f Int32) => Eq   (PrimaryKey OrderT f)

deriving instance Show (Columnar f Int32) => Show (PrimaryKey ShippingInfoT f)
deriving instance Eq   (Columnar f Int32) => Eq   (PrimaryKey ShippingInfoT f)

type Order        = OrderT Identity
type ShippingInfo = ShippingInfoT Identity

{-------------------------------------------------------------------------------
  New table: 'LineItem' (many-to-many relation)
-------------------------------------------------------------------------------}

{-# ANN type LineItemT largeRecordStrict #-}
data LineItemT (f :: Type -> Type) = LineItem {
      lineItemInOrder    :: PrimaryKey OrderT f
    , lineItemForProduct :: PrimaryKey ProductT f
    , lineItemQuantity   :: Columnar f Int32
    }
  deriving (Show, Eq)
  deriving anyclass (Beamable)

type LineItem = LineItemT Identity

instance Table LineItemT where
    data PrimaryKey LineItemT f =
        LineItemId (PrimaryKey OrderT f) (PrimaryKey ProductT f)
      deriving stock (GHC.Generic)
      deriving anyclass (Beamable)

    primaryKey l = LineItemId l.lineItemInOrder l.lineItemForProduct

{-------------------------------------------------------------------------------
  Version 3 of the DB
-------------------------------------------------------------------------------}

{-# ANN type ShoppingCart3Db largeRecordStrict #-}
data ShoppingCart3Db (f :: Type -> Type) = ShoppingCart3Db {
      shoppingCart3Users         :: f (TableEntity UserT)
    , shoppingCart3UserAddresses :: f (TableEntity AddressT)
    , shoppingCart3Products      :: f (TableEntity ProductT)
    , shoppingCart3Orders        :: f (TableEntity OrderT)
    , shoppingCart3ShippingInfos :: f (TableEntity ShippingInfoT)
    , shoppingCart3LineItems     :: f (TableEntity LineItemT)
    }

instance Database be ShoppingCart3Db

{-------------------------------------------------------------------------------
  DB settings
-------------------------------------------------------------------------------}

shoppingCart3Db :: DatabaseSettings be ShoppingCart3Db
shoppingCart3Db = defaultDbSettings `withDbModification`
    dbModification{shoppingCart3UserAddresses =
                         setEntityName "addresses"
                      <> modifyTableFields tableModification{addressLine1 = "address1"
                                                            ,addressLine2 = "address2"
                                                            }
                  , shoppingCart3Products =
                         setEntityName "products"
                  , shoppingCart3Orders =
                         setEntityName "orders"
                      <> modifyTableFields tableModification{orderShippingInfo = ShippingInfoId "shipping_info__id"}
                  , shoppingCart3ShippingInfos =
                         setEntityName "shipping_info"
                      <> modifyTableFields tableModification{shippingInfoId             = "id"
                                                            ,shippingInfoCarrier        = "carrier"
                                                            ,shippingInfoTrackingNumber = "tracking_number"
                                                            }
                  , shoppingCart3LineItems =
                         setEntityName "line_items"
                  }

{-------------------------------------------------------------------------------
  Lenses
-------------------------------------------------------------------------------}

lensesLineItemT :: LineItemT (Lenses LineItemT f)
lensesProductT  :: ProductT  (Lenses ProductT  f)
lensesOrderT    :: OrderT    (Lenses OrderT    f)

lensesLineItemT = tableLenses
lensesProductT  = tableLenses
lensesOrderT    = tableLenses

lensesShoppingCart3 :: ShoppingCart3Db (TableLens f ShoppingCart3Db)
lensesShoppingCart3 = dbLenses

LineItem {
    lineItemInOrder    = OrderId   (LensFor xlineItemInOrder)
  , lineItemForProduct = ProductId (LensFor xlineItemForProduct)
  , lineItemQuantity   = LensFor xlineItemQuantity
  } = lensesLineItemT

Product {
    productId          = LensFor xproductId
  , productTitle       = LensFor xproductTitle
  , productDescription = LensFor xproductDescription
  , productPrice       = LensFor xproductPrice
  } = lensesProductT

Order {
    orderId            = LensFor xorderId
  , orderDate          = LensFor xorderDate
  , orderForUser       = UserId         (LensFor xorderForUser)
  , orderShipToAddress = AddressId      (LensFor xorderShipToAddress)
  , orderShippingInfo  = ShippingInfoId (LensFor xorderShippingInfo)
  } = lensesOrderT

ShoppingCart3Db {
    shoppingCart3Users         = TableLens xshoppingCart3Users
  , shoppingCart3UserAddresses = TableLens xshoppingCart3UserAddresses
  , shoppingCart3Products      = TableLens xshoppingCart3Products
  , shoppingCart3Orders        = TableLens xshoppingCart3Orders
  , shoppingCart3ShippingInfos = TableLens xshoppingCart3ShippingInfos
  , shoppingCart3LineItems     = TableLens xshoppingCart3LineItems
  } = lensesShoppingCart3

-- | Lens from 'Order' to the primary key of the shipping info
--
-- Note that nullability translates to 'Maybe'.
shippingInfo :: Lens' Order (Maybe Int32)
shippingInfo = xorderShippingInfo

{-------------------------------------------------------------------------------
  Tests proper
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.Record.Beam.Tutorial3" [
      testCase "lensToNullableField" test_lensToNullableField
    , testCase "SQL"                 test_SQL
    ]

test_lensToNullableField :: Assertion
test_lensToNullableField = do
    now <- zonedTimeToLocalTime <$> getZonedTime
    assertEqual "Just"    (Just 3) (exampleOrder1 now ^. shippingInfo)
    assertEqual "Nothing" Nothing  (exampleOrder2 now ^. shippingInfo)
    assertEqual "set" (exampleOrder1 now) $
      (exampleOrder2 now) & shippingInfo .~ Just 3
  where
    exampleOrder1, exampleOrder2 :: LocalTime -> Order
    exampleOrder1 now = Order {
          orderId            = 1
        , orderDate          = now
        , orderForUser       = UserId "a@b.c"
        , orderShipToAddress = AddressId 1
        , orderShippingInfo  = ShippingInfoId (Just 3)
        }
    exampleOrder2 now = Order {
          orderId            = 1
        , orderDate          = now
        , orderForUser       = UserId "a@b.c"
        , orderShipToAddress = AddressId 1
        , orderShippingInfo  = ShippingInfoId Nothing
        }

test_SQL :: Assertion
test_SQL = runInMemory $ \conn -> do
    liftIO $ SQLite.execute_ conn $
      "CREATE TABLE cart3_users (email VARCHAR NOT NULL, first_name VARCHAR NOT NULL, last_name VARCHAR NOT NULL, password VARCHAR NOT NULL, PRIMARY KEY( email ));"
    liftIO $ SQLite.execute_ conn $
      "CREATE TABLE addresses ( id INTEGER PRIMARY KEY AUTOINCREMENT, address1 VARCHAR NOT NULL, address2 VARCHAR, city VARCHAR NOT NULL, state VARCHAR NOT NULL, zip VARCHAR NOT NULL, for_user__email VARCHAR NOT NULL );"
    liftIO $ SQLite.execute_ conn $
      "CREATE TABLE products ( id INTEGER PRIMARY KEY AUTOINCREMENT, title VARCHAR NOT NULL, description VARCHAR NOT NULL, price INT NOT NULL );"
    liftIO $ SQLite.execute_ conn $
      "CREATE TABLE orders ( id INTEGER PRIMARY KEY AUTOINCREMENT, date TIMESTAMP NOT NULL, for_user__email VARCHAR NOT NULL, ship_to_address__id INT NOT NULL, shipping_info__id INT);"
    liftIO $ SQLite.execute_ conn $
      "CREATE TABLE shipping_info ( id INTEGER PRIMARY KEY AUTOINCREMENT, carrier VARCHAR NOT NULL, tracking_number VARCHAR NOT NULL);"
    liftIO $ SQLite.execute_ conn $
      "CREATE TABLE line_items (item_in_order__id INTEGER NOT NULL, item_for_product__id INTEGER NOT NULL, item_quantity INTEGER NOT NULL)"

    (jamesAddress1, bettyAddress1, _bettyAddress2, redBall, mathTextbook, introToHaskell, _suitcase) <- do
      runInsert $ insert (shoppingCart3Db ^. xshoppingCart3Users) $
        insertValues users

      [jamesAddress1, bettyAddress1, bettyAddress2] <-
        runInsertReturningList $
          insertReturning (shoppingCart3Db ^. xshoppingCart3UserAddresses) $
            insertExpressions addresses

      [redBall, mathTextbook, introToHaskell, suitcase] <-
        runInsertReturningList $
          insertReturning (shoppingCart3Db ^. xshoppingCart3Products) $
            insertExpressions products

      pure ( jamesAddress1, bettyAddress1, bettyAddress2, redBall, mathTextbook, introToHaskell, suitcase )

    -- Check autoincrement field
    liftIO $ assertEqual "jamesAddress1" addr1 jamesAddress1

    -- Marshalling custom types
    bettyShippingInfo <- do
      [bettyShippingInfo] <-
        runInsertReturningList $
          insertReturning (shoppingCart3Db ^. xshoppingCart3ShippingInfos) $
            insertExpressions [
                ShippingInfo default_ (val_ USPS) (val_ "12345790ABCDEFGHI")
              ]
      pure bettyShippingInfo
    liftIO $ assertEqual "bettyShippingInfo" info1 bettyShippingInfo

    -- Timestamps
    now <- liftIO $ zonedTimeToLocalTime <$> getZonedTime
    [jamesOrder1, bettyOrder1, jamesOrder2] <-
      runInsertReturningList $
        insertReturning (shoppingCart3Db ^. xshoppingCart3Orders) $
          insertExpressions [
              Order default_ currentTimestamp_ (val_ (pk james)) (val_ (pk jamesAddress1)) nothing_
            , Order default_ currentTimestamp_ (val_ (pk betty)) (val_ (pk bettyAddress1)) (just_ (val_ (pk bettyShippingInfo)))
            , Order default_ currentTimestamp_ (val_ (pk james)) (val_ (pk jamesAddress1)) nothing_
            ]
    -- Less than one second should have passed in between us taking a timestamp
    -- and sqlite actually creating the row
    liftIO $ assertBool "timestamp" $
      nominalDiffTimeToSeconds ((jamesOrder1 ^. xorderDate) `diffLocalTime` now) < 1

    -- Create line items
    let lineItems :: [LineItem]
        lineItems = [
            LineItem (pk jamesOrder1) (pk redBall)        10
          , LineItem (pk jamesOrder1) (pk mathTextbook)   1
          , LineItem (pk jamesOrder1) (pk introToHaskell) 4
          , LineItem (pk bettyOrder1) (pk mathTextbook)   3
          , LineItem (pk bettyOrder1) (pk introToHaskell) 3
          , LineItem (pk jamesOrder2) (pk mathTextbook)   1
          ]
    runInsert $ insert (shoppingCart3Db ^. xshoppingCart3LineItems) $
      insertValues lineItems

    -- LEFT JOIN: Users and orders
    usersAndOrders <-
      runSelectReturningList $
        select $ do
          user  <- all_ (shoppingCart3Db ^. xshoppingCart3Users)
          order <- leftJoin_ (all_ (shoppingCart3Db ^. xshoppingCart3Orders)) (\order -> order.orderForUser `references_` user)
          pure (user, order)

    let expectedUsersAndOrders :: [(User, Maybe Order)]
        expectedUsersAndOrders = [
            (james, Just jamesOrder1)
          , (james, Just jamesOrder2)
          , (betty, Just bettyOrder1)
          , (sam, Nothing)
          ]

    liftIO $ assertEqual "usersAndOrders" expectedUsersAndOrders usersAndOrders

    -- LEFT JOIN: Users without any orders
    usersWithNoOrders <-
      runSelectReturningList $
        select $ do
          user  <- all_ (shoppingCart3Db ^. xshoppingCart3Users)
          order <- leftJoin_ (all_ (shoppingCart3Db ^. xshoppingCart3Orders)) (\order -> order.orderForUser `references_` user)
          guard_ (isNothing_ order)
          pure user
    liftIO $ assertEqual "usersWithNoOrders" [sam] usersWithNoOrders

    -- Alternative way to get users with no orders
    usersWithNoOrders' <-
      runSelectReturningList $
        select $ do
          user  <- all_ (shoppingCart3Db ^. xshoppingCart3Users)
          guard_ (not_ (exists_ (filter_ (\order -> order.orderForUser `references_` user) (all_ (shoppingCart3Db ^. xshoppingCart3Orders)))))
          pure user

    liftIO $ assertEqual "usersWithNoOrders'" [sam] usersWithNoOrders'

    -- Aggregates
    ordersWithCostOrdered <-
      runSelectReturningList $
        select $ orderBy_ (\(_order, total) -> desc_ total)
               $ aggregate_ (\(order, lineItem, product) ->
                       (group_ order, sum_ (lineItem ^. xlineItemQuantity * product ^. xproductPrice)))
               $ do
          lineItem <- all_     (shoppingCart3Db ^. xshoppingCart3LineItems)
          order    <- related_ (shoppingCart3Db ^. xshoppingCart3Orders)   lineItem.lineItemInOrder
          product  <- related_ (shoppingCart3Db ^. xshoppingCart3Products) lineItem.lineItemForProduct
          pure (order, lineItem, product)

    let totalJamesOrder1, totalJamesOrder2, totalBettyOrder1 :: Int32
        totalJamesOrder1 = 10 * redBall.productPrice
                         +  1 * mathTextbook.productPrice
                         +  4 * introToHaskell.productPrice
        totalJamesOrder2 =  1 * mathTextbook.productPrice
        totalBettyOrder1 =  3 * mathTextbook.productPrice
                         +  3 * introToHaskell.productPrice

        expectedOrdersWithCostOrdered :: [(Order, Maybe Int32)]
        expectedOrdersWithCostOrdered = [
            (jamesOrder1, Just totalJamesOrder1)
          , (bettyOrder1, Just totalBettyOrder1)
          , (jamesOrder2, Just totalJamesOrder2)
          ]

    liftIO $ assertEqual "ordersWithCostOrdered" expectedOrdersWithCostOrdered ordersWithCostOrdered

    -- LEFT JOIN: Using 'maybe_'
    allUsersAndTotals <-
      runSelectReturningList $
        select $ orderBy_ (\(_user, total) -> desc_ total)
               $ aggregate_ (\(user, lineItem, product) ->
                       (group_ user, sum_ (maybe_ 0 id lineItem.lineItemQuantity * maybe_ 0 id (product ^. xproductPrice))))
               $ do
          user     <- all_ (shoppingCart3Db ^. xshoppingCart3Users)
          order    <- leftJoin_ (all_ (shoppingCart3Db ^. xshoppingCart3Orders))
                                (\order -> order.orderForUser `references_` user)
          lineItem <- leftJoin_ (all_ (shoppingCart3Db ^. xshoppingCart3LineItems))
                                (\lineItem -> maybe_ (val_ False) (\order' -> lineItem.lineItemInOrder `references_` order') order)
          product  <- leftJoin_ (all_ (shoppingCart3Db ^. xshoppingCart3Products))
                                (\product -> maybe_ (val_ False) (\lineItem' -> lineItem'.lineItemForProduct `references_` product) lineItem)
          pure (user, lineItem, product)

    -- Bug in beam? Original tutorial (without LR) has same problem.
    -- <https://github.com/haskell-beam/beam/issues/580>
    let expectedAllUsersAndTotals :: [(User, Maybe Int32)]
        expectedAllUsersAndTotals = [
            (betty, Just totalBettyOrder1)
          , (james, Just 0) -- TODO: Should be (totalJamesOrder1 + totalJamesOrder2))
          , (sam,   Just 0)
          ]

    liftIO $ assertEqual "allUsersAndTotals" expectedAllUsersAndTotals allUsersAndTotals

    -- LEFT JOIN: Using 'leftJoin_''
    allUsersAndTotals2 <-
      runSelectReturningList $
        select $ orderBy_ (\(_user, total) -> desc_ total)
               $ aggregate_ (\(user, lineItem, product) ->
                       (group_ user, sum_ (maybe_ 0 id lineItem.lineItemQuantity * maybe_ 0 id (product ^. xproductPrice))))
               $ do
          user     <- all_ (shoppingCart3Db ^. xshoppingCart3Users)
          order    <- leftJoin_  (all_ (shoppingCart3Db ^. xshoppingCart3Orders))
                                 (\order -> order.orderForUser `references_` user)
          lineItem <- leftJoin_' (all_ (shoppingCart3Db ^. xshoppingCart3LineItems))
                                 (\lineItem -> just_ lineItem.lineItemInOrder ==?. pk order)
          product  <- leftJoin_' (all_ (shoppingCart3Db ^. xshoppingCart3Products))
                                 (\product -> lineItem.lineItemForProduct ==?. just_ (pk product))
          pure (user, lineItem, product)

    let expectedAllUsersAndTotals2 :: [(User, Maybe Int32)]
        expectedAllUsersAndTotals2 = [
            (james, Just (totalJamesOrder1 + totalJamesOrder2))
          , (betty, Just totalBettyOrder1)
          , (sam,   Just 0)
          ]

    liftIO $ assertEqual "allUsersAndTotals2" expectedAllUsersAndTotals2 allUsersAndTotals2

    -- Dealing with nullable foreign keys
    allUnshippedOrders <-
      runSelectReturningList $
        select $ filter_ (\info -> isNothing_ info.orderShippingInfo)
               $ all_ (shoppingCart3Db ^. xshoppingCart3Orders)

    let expectedAllUnshippedOrders :: [Order]
        expectedAllUnshippedOrders = [jamesOrder1, jamesOrder2]

    liftIO $ assertEqual "allUnshippedOrders" expectedAllUnshippedOrders allUnshippedOrders

    -- .. in combination with aggregation
    shippingInformationByUser <-
      runSelectReturningList $
        select $ aggregate_ (\(user, order) ->
                   let ShippingInfoId siId = order.orderShippingInfo
                   in ( group_ user
                      , as_ @Int32 $ count_ (as_ @(Maybe Int32) (maybe_ (just_ 1) (\_ -> nothing_) siId))
                      , as_ @Int32 $ count_ siId
                      ))

               $ do
          user  <- all_ (shoppingCart3Db ^. xshoppingCart3Users)
          order <- leftJoin_ (all_ (shoppingCart3Db ^. xshoppingCart3Orders)) (\order -> order.orderForUser `references_` user)
          pure (user, order)

    let expectedShippingInformationByUser :: [(User, Int32, Int32)]
        expectedShippingInformationByUser = [
              (betty, 0, 1)
            , (james, 2, 0)
            , (sam,   1, 0) -- Incorrect value (expected problem, mentioned in tutorial)
            ]

    liftIO $ assertEqual "shippingInformationByUser"
               expectedShippingInformationByUser
               shippingInformationByUser

    -- Using implicit subselects

    shippingInformationByUser' <-
      runSelectReturningList $
        select $ do
          forUser <- all_ (shoppingCart3Db ^. xshoppingCart3Users)

          (email, unshippedCount) <-
            aggregate_ (\(email, _order) -> (group_ email, countAll_)) $
            do user  <- all_ (shoppingCart3Db ^. xshoppingCart3Users)
               order <- leftJoin_ (all_ (shoppingCart3Db ^. xshoppingCart3Orders))
                                  (\order -> order.orderForUser `references_` user &&. isNothing_ order.orderShippingInfo)
               pure (pk user, order)

          guard_ (email `references_` forUser)

          (email', shippedCount) <-
            aggregate_ (\(email', _order) -> (group_ email', countAll_)) $
            do user  <- all_ (shoppingCart3Db ^. xshoppingCart3Users)
               order <- leftJoin_ (all_ (shoppingCart3Db ^. xshoppingCart3Orders))
                                  (\order -> order.orderForUser `references_` user &&. isJust_ order.orderShippingInfo)
               pure (pk user, order)

          guard_ (email' `references_` forUser)

          pure (forUser, unshippedCount, shippedCount)

    -- TODO: These numbers make no sense
    -- <https://github.com/haskell-beam/beam/issues/580>
    let expectedShippingInformationByUser' :: [(User, Int32, Int32)]
        expectedShippingInformationByUser' = [
              (betty, 1, 1)
            , (james, 2, 1)
            , (sam,   1, 1)
            ]

    liftIO $ assertEqual "shippingInformationByUser'"
               expectedShippingInformationByUser'
               shippingInformationByUser'

    -- Alternative formulation: using explicit subselects
    shippingInformationByUser'' <-
        runSelectReturningList $
        select $
        do forUser <- all_ (shoppingCart3Db ^. xshoppingCart3Users)

           (email, unshippedCount) <-
             subselect_ $
             aggregate_ (\(email, _order) -> (group_ email, countAll_)) $
             do user  <- all_ (shoppingCart3Db ^. xshoppingCart3Users)
                order <- leftJoin_ (all_ (shoppingCart3Db ^. xshoppingCart3Orders))
                                   (\order -> order.orderForUser `references_` user &&. isNothing_ order.orderShippingInfo)
                pure (pk user, order)

           guard_ (email `references_` forUser)

           (email', shippedCount) <-
             subselect_ $
             aggregate_ (\(email', _order) -> (group_ email', countAll_)) $
             do user  <- all_ (shoppingCart3Db ^. xshoppingCart3Users)
                order <- leftJoin_ (all_ (shoppingCart3Db ^. xshoppingCart3Orders))
                                   (\order -> order.orderForUser `references_` user &&. isJust_ order.orderShippingInfo)
                pure (pk user, order)
           guard_ (email' `references_` forUser)

           pure (forUser, unshippedCount, shippedCount)

    liftIO $ assertEqual "shippingInformationByUser''"
               expectedShippingInformationByUser'
               shippingInformationByUser''
  where
    users :: [User]
    users@[james, betty, sam] = [
          User "james@example.com" "James" "Smith"  "b4cc344d25a2efe540adbf2678e2304c"
        , User "betty@example.com" "Betty" "Jones"  "82b054bd83ffad9b6cf8bdb98ce3cc2f"
        , User "sam@example.com"   "Sam"   "Taylor" "332532dcfaa1cbf61e2a266bd723612c"
        ]

    addresses :: [AddressT (QExpr Sqlite s)]
    addresses = [
          Address default_ (val_ "123 Little Street")  (val_ Nothing)        (val_ "Boston")    (val_ "MA") (val_ "12345") (val_ (pk james))
        , Address default_ (val_ "222 Main Street")    (val_ (Just "Ste 1")) (val_ "Houston")   (val_ "TX") (val_ "8888")  (val_ (pk betty))
        , Address default_ (val_ "9999 Residence Ave") (val_ Nothing)        (val_ "Sugarland") (val_ "TX") (val_ "8989")  (val_ (pk betty))
        ]

    products :: [ProductT (QExpr Sqlite s)]
    products = [
          Product default_ (val_ "Red Ball")         (val_ "A bright red, very spherical ball")                      (val_ 1000)
        , Product default_ (val_ "Math Textbook")    (val_ "Contains a lot of important math theorems and formulae") (val_ 2500)
        , Product default_ (val_ "Intro to Haskell") (val_ "Learn the best programming language in the world")       (val_ 3000)
        , Product default_ (val_ "Suitcase")               "A hard durable suitcase"                                       15000
        ]

    addr1 :: Address
    addr1 = Address 1 "123 Little Street"  Nothing "Boston" "MA" "12345" (pk james)

    info1 :: ShippingInfo
    info1 = ShippingInfo 1 USPS "12345790ABCDEFGHI"









