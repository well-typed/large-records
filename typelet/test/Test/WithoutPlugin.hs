{-# OPTIONS_GHC -ddump-ds-preopt -ddump-ds -ddump-simpl -ddump-to-file #-}

-- | Show that we do not /need/ the plugin; it's just more convenient.
module Test.WithoutPlugin where

import Data.Kind
import Data.Type.Equality

import Test.Infra

data LetT :: a -> Type where
  LetT :: (b :~: a) -> LetT a

-- The NOINLINE pragma is essential, otherwise the inliner just undoes our work
{-# NOINLINE letT #-}
letT :: LetT a
letT = LetT Refl

castSingleLet :: Int -> Int
castSingleLet x =
    case letT of { LetT (p :: b :~: Int) ->
      let x' :: b
          x' = case p of Refl -> x
      in case p of Refl -> x'
    }

-- | Correct version (albeit somewhat clunky)
hlist1 :: HList '[A, B, C]
hlist1 =
   case letT of { LetT (p2 :: r2 :~: (C : '[])) ->
   case letT of { LetT (p1 :: r1 :~: (B : r2 )) ->
   case letT of { LetT (p0 :: r0 :~: (A : r1 )) ->

     let xs2 :: HList r2
         xs1 :: HList r1
         xs0 :: HList r0

         xs2 = case p2 of Refl -> HCons C HNil
         xs1 = case p1 of Refl -> HCons B xs2
         xs0 = case p0 of Refl -> HCons A xs1

     in case p0 of { Refl ->
        case p1 of { Refl ->
        case p2 of { Refl ->
          xs0
        }}}

   }}}

-- | Unpacking the equalities in the wrong order (leading to quadratic code)
hlist2 :: HList '[A, B, C]
hlist2 =
   case letT of { LetT (p2 :: r2 :~: (C : '[])) ->
   case letT of { LetT (p1 :: r1 :~: (B : r2 )) ->
   case letT of { LetT (p0 :: r0 :~: (A : r1 )) ->

     let xs2 :: HList r2
         xs1 :: HList r1
         xs0 :: HList r0

         xs2 = case p2 of Refl -> HCons C HNil
         xs1 = case p1 of Refl -> HCons B xs2
         xs0 = case p0 of Refl -> HCons A xs1

     in case p2 of { Refl ->
        case p1 of { Refl ->
        case p0 of { Refl ->
          xs0
        }}}

   }}}

-- | Just for completeness, a version where /we/ construct equality proofs
hlist3 :: HList '[A, B, C]
hlist3 =
   case letT of { LetT (p2 :: r2 :~: (C : '[])) ->
   case letT of { LetT (p1 :: r1 :~: (B : r2 )) ->
   case letT of { LetT (p0 :: r0 :~: (A : r1 )) ->

     let xs2 :: HList r2
         xs1 :: HList r1
         xs0 :: HList r0

         xs2 = castWith (cong (sym p2)) (HCons C HNil)
         xs1 = castWith (cong (sym p1)) (HCons B xs2)
         xs0 = castWith (cong (sym p0)) (HCons A xs1)

     in castWith (cong (comp p0 (comp p1 p2))) xs0

   }}}

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

cong :: (a :~: b) -> f a :~: f b
cong Refl = Refl

comp :: (a :~: x : b) -> b :~: c -> a :~: x : c
comp Refl Refl = Refl

