{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fplugin=Data.Record.Anonymous.Plugin #-}
{-# OPTIONS -Wno-orphans #-}

module Test.Prop.Record.Model.Generator (
    -- * Existential wrapper around 'ModelRecord' that hides the record shape
    SomeFields(..)
  , SomeRecord(..)
  , SomeRecordPair(..)
    -- * Construction
  , someModlRecord
  , someAnonRecord
    -- * Mapping
  , onModlRecord
  , onModlRecordM
  , onModlRecordPair
  , onModlRecordPairM
  , onAnonRecord
  , onAnonRecordM
  , onAnonRecordPair
  , onAnonRecordPairM
  ) where

import Data.Proxy
import Data.SOP (NP(..), SListI, All)
import Data.SOP.BasicFunctors

import Data.Record.Anonymous.Advanced (Record, Pair((:=)))
import qualified Data.Record.Anonymous.Advanced as Anon

import Test.QuickCheck

import Test.Prop.Record.Model (
    ModelRecord(..)
  , ModelFields(..)
  , ModelSatisfies
  , Types
  )

import qualified Test.Prop.Record.Model as Model

{-------------------------------------------------------------------------------
  Existential wrapper around 'ModelRecord' that hides the record shape
-------------------------------------------------------------------------------}

data SomeFields where
  SF :: SListI (Types r) => ModelFields r -> SomeFields

data SomeRecord f where
  SR :: SListI (Types r)
     => ModelFields r -> ModelRecord f r -> SomeRecord f

data SomeRecordPair f g where
  SR2 :: SListI (Types r)
      => ModelFields r
      -> ModelRecord f r -> ModelRecord g r -> SomeRecordPair f g

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

someModlRecord ::
     SomeFields
  -> (forall r. SListI (Types r) => ModelFields r -> ModelRecord f r)
  -> SomeRecord f
someModlRecord (SF mf) f = SR mf (f mf)

someAnonRecord :: forall c f.
     ModelSatisfies c
  => Proxy c
  -> SomeFields
  -> (forall r. (Anon.KnownFields r, Anon.AllFields r c) => Record f r)
  -> SomeRecord f
someAnonRecord _ (SF mf) f = SR mf (Model.fromRecord mf $ f' mf)
  where
    f' :: ModelFields r -> Record f r
    f' MF0  = f
    f' MF1  = f
    f' MF2  = f
    f' MF2' = f

{-------------------------------------------------------------------------------
  Mapping
-------------------------------------------------------------------------------}

onModlRecord ::
     ModelSatisfies c
  => Proxy c
  -> (forall r. All c (Types r) => ModelRecord f r -> ModelRecord g r)
  -> SomeRecord f -> SomeRecord g
onModlRecord p f = unI . onModlRecordM p (I . f)

onModlRecordPair ::
     ModelSatisfies c
  => Proxy c
  -> ( forall r.
            All c (Types r)
         => ModelRecord f r -> ModelRecord g r -> ModelRecord h r
     )
  -> SomeRecordPair f g -> SomeRecord h
onModlRecordPair p f = unI . onModlRecordPairM p (I .: f)

onModlRecordM ::
     (Functor m, ModelSatisfies c)
  => Proxy c
  -> (forall r. All c (Types r) => ModelRecord f r -> m (ModelRecord g r))
  -> SomeRecord f -> m (SomeRecord g)
onModlRecordM p f (SR mf r) = Model.satisfyAll p mf $ SR mf <$> f r

onModlRecordPairM ::
     (Functor m, ModelSatisfies c)
  => Proxy c
  -> ( forall r.
            All c (Types r)
         => ModelRecord f r -> ModelRecord g r -> m (ModelRecord h r)
     )
  -> SomeRecordPair f g -> m (SomeRecord h)
onModlRecordPairM p f (SR2 mf r r') = Model.satisfyAll p mf $ SR mf <$> f r r'

onAnonRecord ::
     ModelSatisfies c
  => Proxy c
  -> (forall r. Anon.AllFields r c => Record f r -> Record g r)
  -> SomeRecord f -> SomeRecord g
onAnonRecord p f = unI . onAnonRecordM p (I . f)

onAnonRecordPair ::
     ModelSatisfies c
  => Proxy c
  -> ( forall r.
            Anon.AllFields r c
         => Record f r -> Record g r -> Record h r
     )
  -> SomeRecordPair f g -> SomeRecord h
onAnonRecordPair p f = unI . onAnonRecordPairM p (I .: f)

onAnonRecordM :: forall m c f g.
     (Functor m, ModelSatisfies c)
  => Proxy c
  -> (forall r. Anon.AllFields r c => Record f r -> m (Record g r))
  -> SomeRecord f -> m (SomeRecord g)
onAnonRecordM p f = \(SR mf r) -> SR mf <$> f' mf r
  where
    f' :: forall r. ModelFields r -> ModelRecord f r -> m (ModelRecord g r)
    f' mf r =
        Model.toRecordOfDicts p mf $
          Model.fromRecord mf <$>
            f (Model.toRecord mf r)

onAnonRecordPairM :: forall m c f g h.
     (Functor m, ModelSatisfies c)
  => Proxy c
  -> ( forall r.
            Anon.AllFields r c
         => Record f r -> Record g r -> m (Record h r)
     )
  -> SomeRecordPair f g -> m (SomeRecord h)
onAnonRecordPairM p f = \(SR2 mf r r') -> SR mf <$> f' mf r r'
  where
    f' :: forall r.
         ModelFields r
      -> ModelRecord f r -> ModelRecord g r -> m (ModelRecord h r)
    f' mf r r' =
        Model.toRecordOfDicts p mf $
          Model.fromRecord mf <$>
            f (Model.toRecord mf r) (Model.toRecord mf r')

{-------------------------------------------------------------------------------
  Generators for ModelRecord for concrete rows
-------------------------------------------------------------------------------}

instance Arbitrary (ModelRecord f '[]) where
  arbitrary = pure $ MR Nil

instance ( Arbitrary (f Bool)
         ) => Arbitrary (ModelRecord f '[ "b" := Bool ]) where
  arbitrary =
          (\x -> MR (x :* Nil))
      <$> arbitrary

  shrink (MR (x :* Nil)) = concat [
        (\x' -> MR (x' :* Nil)) <$> shrink x
      ]

instance ( Arbitrary (f Word)
         , Arbitrary (f Bool)
         ) => Arbitrary (ModelRecord f [ "a" := Word, "b" := Bool ]) where
  arbitrary =
          (\x y -> MR (x :* y :* Nil))
      <$> arbitrary
      <*> arbitrary

  shrink (MR (x :* y :* Nil)) = concat [
        (\x' -> MR (x' :* y  :* Nil)) <$> shrink x
      , (\y' -> MR (x  :* y' :* Nil)) <$> shrink y
      ]

instance ( Arbitrary (f Word)
         , Arbitrary (f Bool)
         ) => Arbitrary (ModelRecord f [ "b" := Word, "a" := Bool ]) where
  arbitrary =
          (\x y -> MR (x :* y :* Nil))
      <$> arbitrary
      <*> arbitrary

  shrink (MR (x :* y :* Nil)) = concat [
        (\x' -> MR (x' :* y  :* Nil)) <$> shrink x
      , (\y' -> MR (x  :* y' :* Nil)) <$> shrink y
      ]

{-------------------------------------------------------------------------------
  Generators for existential wrappers
-------------------------------------------------------------------------------}

instance Arbitrary SomeFields where
  arbitrary = elements [
        SF MF0
      , SF MF1
      , SF MF2
      , SF MF2'
      ]

  shrink (SF MF0)  = []
  shrink (SF MF1)  = [SF MF0]
  shrink (SF MF2)  = [SF MF1]
  shrink (SF MF2') = [SF MF1]

instance ( Arbitrary (f Word), Arbitrary (f Bool)
         ) => Arbitrary (SomeRecord f) where
  arbitrary = oneof [
        SR MF0  <$> arbitrary
      , SR MF1  <$> arbitrary
      , SR MF2  <$> arbitrary
      , SR MF2' <$> arbitrary
      ]

  shrink (SR MF0 r) = concat [
        SR MF0 <$> shrink r
      ]
  shrink (SR MF1 r) = concat [
        SR MF1 <$> shrink r
      , pure $ SR MF0 (dropHead r)
      ]
  shrink (SR MF2 r) = concat [
        SR MF2 <$> shrink r
      , pure $ SR MF1 (dropHead r)
      ]
  shrink (SR MF2' r) = concat [
        SR MF2' <$> shrink r
        -- can't remove a field here (at least, not easily)
      ]

instance ( Arbitrary (f Word), Arbitrary (f Bool)
         , Arbitrary (g Word), Arbitrary (g Bool)
         ) => Arbitrary (SomeRecordPair f g) where
  arbitrary = oneof [
        SR2 MF0  <$> arbitrary <*> arbitrary
      , SR2 MF1  <$> arbitrary <*> arbitrary
      , SR2 MF2  <$> arbitrary <*> arbitrary
      , SR2 MF2' <$> arbitrary <*> arbitrary
      ]

  shrink (SR2 MF0 r r') = concat [
        SR2 MF0 <$> shrink r <*> pure   r'
      , SR2 MF0 <$> pure   r <*> shrink r'
      ]
  shrink (SR2 MF1 r r') = concat [
        SR2 MF1 <$> shrink r <*> pure   r'
      , SR2 MF1 <$> pure   r <*> shrink r'
      , pure $ SR2 MF0 (dropHead r) (dropHead r')
      ]
  shrink (SR2 MF2 r r') = concat [
        SR2 MF2 <$> shrink r <*> pure   r'
      , SR2 MF2 <$> pure   r <*> shrink r'
      , pure $ SR2 MF1 (dropHead r) (dropHead r')
      ]
  shrink (SR2 MF2' r r') = concat [
        SR2 MF2' <$> shrink r <*> pure   r'
      , SR2 MF2' <$> pure   r <*> shrink r'
        -- can't remove a field here (at least, not easily)
      ]

{-------------------------------------------------------------------------------
  Show/Eq instances
-------------------------------------------------------------------------------}

deriving instance Show SomeFields

instance ( Show (f Word), Show (f Bool)
         ) => Show (SomeRecord f) where
  show (SR MF0  r) = show r
  show (SR MF1  r) = show r
  show (SR MF2  r) = show r
  show (SR MF2' r) = show r

instance ( Show (f Word), Show (f Bool)
         , Show (g Word), Show (g Bool)
         ) => Show (SomeRecordPair f g) where
  show (SR2 MF0  r r') = show (r, r')
  show (SR2 MF1  r r') = show (r, r')
  show (SR2 MF2  r r') = show (r, r')
  show (SR2 MF2' r r') = show (r, r')

instance ( Eq (f Word), Eq (f Bool)
         ) => Eq (SomeRecord f) where
  x == y =
      case (x, y) of
        (SR MF0  r , SR MF0  r') -> r == r'
        (SR MF1  r , SR MF1  r') -> r == r'
        (SR MF2  r , SR MF2  r') -> r == r'
        (SR MF2' r , SR MF2' r') -> r == r'
        (_         , _         ) -> False
    where
      -- Make sure we don't omit a case above
      _coverAllCases :: ()
      _coverAllCases =
          case x of
            SR MF0  _ -> ()
            SR MF1  _ -> ()
            SR MF2  _ -> ()
            SR MF2' _ -> ()

instance ( Eq (f Word), Eq (f Bool)
         , Eq (g Word), Eq (g Bool)
         ) => Eq (SomeRecordPair f g) where
  x == y =
      case (x, y) of
        (SR2 MF0  r1 r2 , SR2 MF0  r1' r2' ) -> r1 == r1' && r2 == r2'
        (SR2 MF1  r1 r2 , SR2 MF1  r1' r2' ) -> r1 == r1' && r2 == r2'
        (SR2 MF2  r1 r2 , SR2 MF2  r1' r2' ) -> r1 == r1' && r2 == r2'
        (SR2 MF2' r1 r2 , SR2 MF2' r1' r2' ) -> r1 == r1' && r2 == r2'
        (_              , _                ) -> False
    where
      -- Make sure we don't omit a case above
      _coverAllCases :: ()
      _coverAllCases =
          case x of
            SR2 MF0  _ _ -> ()
            SR2 MF1  _ _ -> ()
            SR2 MF2  _ _ -> ()
            SR2 MF2' _ _ -> ()

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

dropHead :: ModelRecord f (n := x ': xs) -> ModelRecord f xs
dropHead (MR (_ :* xs)) = MR xs

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(f .: g) x y = f (g x y)

