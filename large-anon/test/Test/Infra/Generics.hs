{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- | Additional generic functions used in tests
module Test.Infra.Generics (
    describeRecord
  , debugFieldTypes
  ) where

import Data.Kind
import Data.List (intercalate)
import Data.Record.Generic
import Data.SOP
import Data.Typeable
import GHC.TypeLits

import qualified Data.Record.Generic.Rep as Rep

import Data.Record.Anon
import Data.Record.Anon.Advanced (Record)

-- | Show type of every field in the record
describeRecord :: forall (a :: Type).
     (Generic a, Constraints a Typeable)
  => Proxy a
  -> String
describeRecord p =
      combine
    . Rep.collapse
    . Rep.cmap (Proxy @Typeable) aux
    $ names
  where
    names :: Rep (K String) a
    names = recordFieldNames $ metadata p

    -- @x@ here will be of the form @f x'@, for some @x'@, and we have a
    -- constraint @Typeable (f x')@ in scope. We therefore do not need to
    -- manually apply @f@ here.
    aux :: forall x. Typeable x => K String x -> K String x
    aux (K name) = K $ name ++ " :: " ++ show (typeRep (Proxy @x))

    combine :: [String] -> String
    combine fs = concat [
          "Record {"
        , intercalate ", " fs
        , "}"
        ]

-- | Like 'describeRecord', but exclusively using type-level information.
--
-- WARNING: The @All@ constraint will lead to quadratic code. This is for
-- debugging only.
debugFieldTypes :: forall f r.
     All IsField (FieldTypes f r)
  => Proxy (Record f r) -> String
debugFieldTypes _ =
    (\str -> "[" ++ str ++ "]") . intercalate "," . hcollapse $
      aux (shape :: Shape (FieldTypes f r))
  where
    aux :: forall fs. All IsField fs => Shape fs -> NP (K String) fs
    aux ShapeNil      = Nil
    aux (ShapeCons s) = name :* aux s

    name :: forall n a. KnownSymbol n => K String '(n, a)
    name = K (symbolVal (Proxy @n))



