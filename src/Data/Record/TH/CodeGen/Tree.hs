{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Record.TH.CodeGen.Tree (
    -- * Trees and forests
    Tree(..)
  , Forest(..)
    -- * Catamorphisms
  , Cata(..)
  , tree
  , forest
    -- * Dealing with @ghc@'s tuple size limit
  , TupleLimit(..)
  , nest
  , mkTupleE
  , mkTupleT
  , mkTupleP
  ) where

import Language.Haskell.TH

import Data.Record.TH.CodeGen.TH

{-------------------------------------------------------------------------------
  Trees and forests
-------------------------------------------------------------------------------}

-- | Trees with values at the leaves
data Tree   a = Leaf a | Branch (Forest a) deriving (Show)
data Forest a = Forest [Tree a]            deriving (Show)

{-------------------------------------------------------------------------------
  Catamorphisms

  Unlike regular folds, these catamorphisms are structure preserving.
  See "Dealing with Large Bananas", by Ralf Lämmel et al
-------------------------------------------------------------------------------}

data Cata a b = Cata {
      leaf   :: a -> b
    , branch :: [b] -> b
    }

tree :: Cata a b -> Tree a -> b
tree cata (Leaf   a)  = leaf   cata a
tree cata (Branch as) = forest cata as

forest :: Cata a b -> Forest a -> b
forest cata (Forest ts) = branch cata (map (tree cata) ts)

{-------------------------------------------------------------------------------
  Nesting
-------------------------------------------------------------------------------}

-- | Observe @ghc@'s tuple length
--
-- Haskell has a limit of 62 fields per tuple. Here we take an arbitrary
-- list and turn it into a nested tuple that preserves this limit.
--
-- Example: if we reduce the limit to @2@, we get the following nestings,
-- for lengths @[1..10]@:
--
-- >     A
-- >    (A, A)
-- >   ((A, A),  A)
-- >   ((A, A), (A, A))
-- >  (((A, A), (A, A)),   A)
-- >  (((A, A), (A, A)),  (A, A))
-- >  (((A, A), (A, A)), ((A, A),  A))
-- >  (((A, A), (A, A)), ((A, A), (A, A)))
-- > ((((A, A), (A, A)), ((A, A), (A, A))),  A)
-- > ((((A, A), (A, A)), ((A, A), (A, A))), (A, A))
nest :: TupleLimit -> [a] -> Forest a
nest mLimit = go . map Leaf
  where
    go :: [Tree a] -> Forest a
    go ts | length ts < limit = Forest ts
          | otherwise         = go (map (Branch . Forest) (chunk limit ts))

    limit :: Int
    limit = case mLimit of
              DefaultGhcTupleLimit -> 62
              MaxTupleElems n      -> n

-- | Maximum number of elements in a tuple
data TupleLimit =
    -- | Default maximum number of elements in a tuple in ghc (62)
    DefaultGhcTupleLimit

    -- | Explicit specified liit
  | MaxTupleElems Int

{-------------------------------------------------------------------------------
  Constructing nested types/values/patterns
-------------------------------------------------------------------------------}

-- | Construct tuple type
mkTupleT :: forall a. (a -> Q Type) -> Forest a -> Q Type
mkTupleT f = forest cata
  where
    cata :: Cata a (Q Type)
    cata = Cata {
          leaf   = f
        , branch = \case [t] -> t
                         ts  -> appsT (tupleT (length ts)) ts
        }

-- | Construct tuple expression
mkTupleE :: forall a. (a -> Q Exp) -> Forest a -> Q Exp
mkTupleE f = forest cata
  where
     cata :: Cata a (Q Exp)
     cata = Cata {
           leaf   = f
         , branch = \case [e] -> e
                          es  -> tupE es
         }

-- | Construct tuple pattern
mkTupleP :: forall a. (a -> Q Pat) -> Forest a -> Q Pat
mkTupleP f = forest cata
  where
    cata :: Cata a (Q Pat)
    cata = Cata {
          leaf   = f
        , branch = \case [p] -> p
                         ps  -> tupP ps
        }

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

chunk :: Int -> [a] -> [[a]]
chunk n = go
  where
    go :: [a] -> [[a]]
    go [] = []
    go xs = let (firstChunk, rest) = splitAt n xs in firstChunk : go rest
