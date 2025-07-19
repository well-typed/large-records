{-# OPTIONS_GHC -fplugin=TypeLet #-}
-- {-# OPTIONS_GHC -ddump-ds-preopt -ddump-ds -ddump-simpl -ddump-to-file #-}

module Test.Sanity (tests) where

import Data.Functor.Identity

import TypeLet

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Test.Infra

tests :: TestTree
tests = testGroup "Test.Sanity" [
      testGroup "simple" [
          testProperty  "reflexive"   $ castIsId castReflexive
        , testProperty  "singleLet"   $ castIsId castSingleLet
        , testProperty  "singleLetAs" $ castIsId castSingleLetAs
        ]
    , testGroup "HList" [
          testCase "Let"          $ testHList hlistLet
        , testCase "LetAs"        $ testHList hlistLetAs
        , testCase "LetAsCPS_bad" $ testHList hlistLetAsCPS_bad
        , testCase "LetAsCPS"     $ testHList hlistLetAsCPS
        ]
    , testGroup "Ap" [
          testCase "Let" $ testAp apLet
        ]
    ]

castIsId :: (Eq a, Show a) => (a -> a) -> a -> Property
castIsId f x = x === f x

testHList :: HList '[A, B, C] -> Assertion
testHList =
    assertEqual "" hlistBaseline

testAp ::
     (forall f r. Applicative f => (A -> B -> C -> r) -> f r)
  -> Assertion
testAp apTest =
    assertEqual "" (runIdentity $ apBaseline f) (runIdentity $ apTest f)
  where
    f :: A -> B -> C -> HList '[A, B, C]
    f x y z = HCons x $ HCons y $ HCons z $ HNil

{-------------------------------------------------------------------------------
  Simple casts
-------------------------------------------------------------------------------}

-- | Trivial test: no let-bounds
--
-- TODO: We should also make sure that non-type correct functions are rejected
-- (they are, just don't have a test for them currently)
castReflexive :: Int -> Int
castReflexive = castEqual

-- | Introduce single let binding, then cast there and back
castSingleLet :: Int -> Int
castSingleLet x =
    case letT (Proxy @Int) of
      LetT (_ :: Proxy t1) ->
        let y :: t1
            y = castEqual x
        in castEqual y

-- | Single let-as
castSingleLetAs :: Identity Int -> Identity Int
castSingleLetAs x =
    case letAs x of
      LetAs (x' :: Identity t1) ->
        castEqual x'

{-------------------------------------------------------------------------------
  Small versions of the 'HList' tests that we use for size measurements

  Since these work with only 3 indices, they are a bit more manageable for
  easy experimentation.
-------------------------------------------------------------------------------}

{-# NOINLINE hlistBaseline #-}
hlistBaseline :: HList '[A, B, C]
hlistBaseline =
      HCons A
    $ HCons B
    $ HCons C
    $ HNil

hlistLet :: HList '[A, B, C]
hlistLet =
    case letT (Proxy @(C : '[])) of { LetT (_ :: Proxy r2) ->
    case letT (Proxy @(B : r2))  of { LetT (_ :: Proxy r1) ->
    case letT (Proxy @(A : r1))  of { LetT (_ :: Proxy r0) ->

     let xs2 :: HList r2
         xs1 :: HList r1
         xs0 :: HList r0

         xs2 = castEqual (HCons C HNil)
         xs1 = castEqual (HCons B xs2)
         xs0 = castEqual (HCons A xs1)

     in castEqual xs0
    }}}

hlistLetAs :: HList '[A, B, C]
hlistLetAs =
    case letAs (HCons C HNil) of { LetAs (xs02 :: HList t02) ->
    case letAs (HCons B xs02) of { LetAs (xs01 :: HList t01) ->
    case letAs (HCons A xs01) of { LetAs (xs00 :: HList t00) ->
      castEqual xs00
    }}}

hlistLetAsCPS_bad :: HList '[A, B, C]
hlistLetAsCPS_bad =
    letAs' (HCons C HNil) $ \(xs02 :: HList t02) ->
    letAs' (HCons B xs02) $ \(xs01 :: HList t01) ->
    letAs' (HCons A xs01) $ \(xs00 :: HList t00) ->
      castEqual xs00

hlistLetAsCPS :: HList '[A, B, C]
hlistLetAsCPS =
    letT' (Proxy @'[A, B, C]) $ \(_ :: Proxy r) -> castEqual $
      letAs' @(HList r) (HCons C HNil) $ \(xs02 :: HList t02) ->
      letAs' @(HList r) (HCons B xs02) $ \(xs01 :: HList t01) ->
      letAs' @(HList r) (HCons A xs01) $ \(xs00 :: HList t00) ->
        castEqual xs00

{-------------------------------------------------------------------------------
  Similarly, small versions of the @<*>@ tests.
-------------------------------------------------------------------------------}

apBaseline :: Applicative f => (A -> B -> C -> r) -> f r
apBaseline f =
        pure f
    <*> pure A
    <*> pure B
    <*> pure C

apLet :: forall f r. Applicative f => (A -> B -> C -> r) -> f r
apLet f =
    case letT (Proxy @(C -> r))   of { LetT (_ :: Proxy l02) ->
    case letT (Proxy @(B -> l02)) of { LetT (_ :: Proxy l01) ->
    case letT (Proxy @(A -> l01)) of { LetT (_ :: Proxy l00) ->

      let f00 :: f l00
          f01 :: f l01
          f02 :: f l02

          res :: f r

          f00 = pure (castEqual f)
          f01 = castEqual f00 <*> pure A
          f02 = castEqual f01 <*> pure B
          res = castEqual f02 <*> pure C

      in res

    }}}
