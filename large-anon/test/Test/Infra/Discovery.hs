{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Test.Infra.Discovery (
     -- * Intersect rows
     InBothRows(..)
   , inLeftRow
   , inRightRow
   , intersectRows
     -- * Check for projection
   , NotSubRow
   , checkIsSubRow
   , maybeProject
     -- * Compute intersection
   , Intersection(..)
   , intersect
     -- * Auxiliary
   , catMaybeF
   , pairFst
   , pairSnd
   ) where

import Data.Kind
import Data.Maybe (catMaybes)
import Data.Typeable
import GHC.TypeLits

import Data.Record.Anon
import Data.Record.Anon.Advanced (Record, InRow(InRow))
import qualified Data.Record.Anon.Advanced as Anon
import Control.Monad.State

{-------------------------------------------------------------------------------
  Intersect rows

  NOTE: A constraint @SubRow r r@ is not completely trivial: it means that
  there are no shadowed fields.
-------------------------------------------------------------------------------}

data InBothRows (r1 :: Row k) (r2 :: Row k) (a :: k) where
  InBothRows :: forall k (n :: Symbol) (r1 :: Row k) (r2 :: Row k) (a :: k).
       ( KnownSymbol n
       , RowHasField n r1 a
       , RowHasField n r2 a
       )
    => Proxy n -> InBothRows r1 r2 a

inLeftRow :: InBothRows r1 r2 a -> InRow r1 a
inLeftRow (InBothRows n) = InRow n

inRightRow :: InBothRows r1 r2 a -> InRow r2 a
inRightRow (InBothRows n) = InRow n

intersectRows :: forall k (r1 :: Row k) (r2 :: Row k) proxy proxy'.
     ( KnownFields r1
     , KnownFields r2
     , SubRow r1 r1
     , SubRow r2 r2
     , AllFields r1 Typeable
     , AllFields r2 Typeable
     )
  => proxy r1 -> proxy' r2 -> Record (Maybe :.: InBothRows r1 r2) r2
intersectRows _ _ =
    go Anon.reifySubRow Anon.reifySubRow
  where
    go :: Record (InRow r1) r1
       -> Record (InRow r2) r2
       -> Record (Maybe :.: InBothRows r1 r2) r2
    go r1 r2 = Anon.cmap (Proxy @Typeable) (findField r1) r2

    findField :: forall x2.
          Typeable x2
       => Record (InRow r1) r1 -> InRow r2 x2 -> (Maybe :.: InBothRows r1 r2) x2
    findField r1 f = Comp $
        findMatch . catMaybes $
          Anon.collapse $ Anon.cmap (Proxy @Typeable) (K . checkIsMatch f) r1

    checkIsMatch :: forall x1 x2.
         (Typeable x1, Typeable x2)
      => InRow r2 x2 -> InRow r1 x1 -> Maybe (InBothRows r1 r2 x2)
    checkIsMatch (InRow x2) (InRow x1) = do
        Refl <- sameSymbol x1 x2
        Refl <- eqT :: Maybe (x1 :~: x2)
        return $ InBothRows x1

    findMatch :: [a] -> Maybe a
    findMatch []  = Nothing
    findMatch [a] = Just a
    findMatch _   = error "intersectRows: error: multiple matches"

{-------------------------------------------------------------------------------
  Check for projection
-------------------------------------------------------------------------------}

-- | Fields that are missing or have the wrong type
--
-- TODO: Ideally we should distinguish between type errors and missing fields.
type NotSubRow = [String]

checkIsSubRow :: forall k (r1 :: Row k) (r2 :: Row k) proxy proxy'.
     ( KnownFields r1
     , KnownFields r2
     , SubRow r1 r1
     , SubRow r2 r2
     , AllFields r1 Typeable
     , AllFields r2 Typeable
     )
  => proxy r1 -> proxy' r2 -> Either NotSubRow (Reflected (SubRow r1 r2))
checkIsSubRow p1 p2 =
     uncurry postprocess . flip runState [] . Anon.sequenceA $
       Anon.zipWith
         checkInLeft
         (intersectRows p1 p2)
         (Anon.reifyKnownFields (Proxy @r2))
  where
    checkInLeft ::
         (Maybe :.: InBothRows r1 r2) x
      -> K String x
      -> (State [String] :.: Maybe :.: InRow r1) x
    checkInLeft (Comp Nothing) (K name) = Comp $ state $ \missing ->
        (Comp Nothing, name : missing)
    checkInLeft (Comp (Just inBoth)) _ = Comp $ state $ \missing ->
        (Comp (Just (inLeftRow inBoth)), missing)

    postprocess ::
         Record (Maybe :.: InRow r1) r2
      -> [String]
      -> Either NotSubRow (Reflected (SubRow r1 r2))
    postprocess matched missing =
        maybe (Left missing) (Right . Anon.reflectSubRow) $
          Anon.sequenceA matched

maybeProject :: forall k (f :: k -> Type) (r1 :: Row k) (r2 :: Row k) proxy.
     ( KnownFields r1
     , KnownFields r2
     , SubRow r1  r1
     , SubRow r2 r2
     , AllFields r1  Typeable
     , AllFields r2 Typeable
     )
  => Record f r1 -> proxy r2 -> Either NotSubRow (Record f r2)
maybeProject r1 p = aux <$> checkIsSubRow r1 p
  where
    aux :: Reflected (SubRow r1 r2) -> Record f r2
    aux Reflected = Anon.project r1

{-------------------------------------------------------------------------------
  Compute intersection
-------------------------------------------------------------------------------}

data Intersection (r1 :: Row k) (r2 :: Row k) where
    Intersection :: forall k (r1 :: Row k) (r2 :: Row k) (ri :: Row k).
         ( KnownFields ri
         , SubRow r1 ri
         , SubRow r2 ri
         )
      => Proxy ri -> Intersection r1 r2

intersect :: forall k (r1 :: Row k) (r2 :: Row k) proxy proxy'.
     ( KnownFields r1
     , KnownFields r2
     , SubRow r1 r1
     , SubRow r2 r2
     , AllFields r1 Typeable
     , AllFields r2 Typeable
     )
  => proxy r1 -> proxy' r2 -> Intersection r1 r2
intersect p1 p2 =
    (\(Anon.SomeRecord r) -> aux $ Anon.map pairSnd r) $
      catMaybeF (intersectRows p1 p2)
  where
    aux :: forall ri.
         KnownFields ri
      => Record (InBothRows r1 r2) ri -> Intersection r1 r2
    aux r =
        case (project1, project2) of
          (Reflected, Reflected) -> Intersection (Proxy @ri)
      where
        project1 :: Reflected (SubRow r1 ri)
        project1 = Anon.reflectSubRow $ Anon.map inLeftRow r

        project2 :: Reflected (SubRow r2 ri)
        project2 = Anon.reflectSubRow $ Anon.map inRightRow r

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

catMaybeF :: KnownFields r => Record (Maybe :.: f) r -> Anon.SomeRecord f
catMaybeF =
      Anon.someRecord
    . catMaybes
    . map distrib
    . Anon.toList
    . Anon.map (K . Some)
  where
    distrib :: (String, Some (Maybe :.: f)) -> Maybe (String, Some f)
    distrib (_, Some (Comp Nothing))   = Nothing
    distrib (n, Some (Comp (Just fx))) = Just (n, Some fx)

pairFst :: Product f g x -> f x
pairFst (Pair fx _) = fx

pairSnd :: Product f g x -> g x
pairSnd (Pair _ gx) = gx


