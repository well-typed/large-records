{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

-- | Generic parsing infrastructure
--
-- TODO: Perhaps we could move this (or some form of this) to ghc-tcplugin-api?
-- (The @typelet@ package could then use it, too.)
module Data.Record.Anonymous.Plugin.Parsing (
    -- * Basic infrastructure
    ParseResult(..)
  , parseAll
  , parseAll'
  , withOrig
    -- * Parsers for specific (but not @large-anon@ specific) constructs
  , parseConstraint
  , parseConstraint'
  , parseCons
  , parseNil
  , parsePair
  ) where

import Control.Monad
import Data.Bifunctor
import Data.Void
import GHC.Stack

import Data.Record.Anonymous.Plugin.GhcTcPluginAPI

{-------------------------------------------------------------------------------
  Basic infrastructure
-------------------------------------------------------------------------------}

data ParseResult e a =
    -- | Parse successful
    ParseOk a

    -- | Different constraint than we're looking for (does not imply an error)
  | ParseNoMatch

    -- | Constraint of the shape we're looking for, but something is wrong
  | ParseError e
  deriving (Functor)

instance Bifunctor ParseResult where
  bimap _ g (ParseOk a)    = ParseOk (g a)
  bimap _ _ ParseNoMatch   = ParseNoMatch
  bimap f _ (ParseError e) = ParseError (f e)

-- | Apply parser to each value in turn, bailing at the first error
parseAll :: forall e a b. (a -> ParseResult e b) -> [a] -> Either e [b]
parseAll f = go []
  where
    go :: [b] -> [a] -> Either e [b]
    go acc []     = Right (reverse acc)
    go acc (a:as) = case f a of
                      ParseOk b    -> go (b:acc) as
                      ParseNoMatch -> go    acc  as
                      ParseError e -> Left e

-- | Variation on 'parseAll' which rules out the error case
parseAll' :: (a -> ParseResult Void b) -> [a] -> [b]
parseAll' f = aux . parseAll f
  where
    aux :: Either Void [b] -> [b]
    aux (Left  v)  = absurd v
    aux (Right bs) = bs

-- | Bundle the parse result with the original value
withOrig :: (a -> ParseResult e b) -> (a -> ParseResult e (a, b))
withOrig f x = fmap (x, ) $ f x

{-------------------------------------------------------------------------------
  Parsers for specific (but not @large-anon@ specific) constructs
-------------------------------------------------------------------------------}

-- | Generic constraint parser
--
-- TODO: If we add some parsing infra to ghc-tcplugin-api, maybe a (form of)
-- this function could live there too.
parseConstraint ::
     HasCallStack
  => (Class -> [Type] -> Maybe a) -- ^ Do we want to try and match against this?
  -> (a -> Maybe b)               -- ^ Parser for the class arguments
  -> Ct                           -- ^ Constraint to parse
  -> ParseResult e (GenLocated CtLoc b)
parseConstraint p f ct = fmap (L $ ctLoc ct) $
    case classifyPredType (ctPred ct) of
      ClassPred cls args | Just a <- p cls args ->
        case f a of
          Just parsed ->
            ParseOk parsed
          Nothing ->
            panic $ concat [
                "Unexpected "
              , showSDocUnsafe (ppr cls)
              , " constraint with arguments:\n"
              , unlines (map (showSDocUnsafe . ppr) args)
              , "\nat\n"
              , prettyCallStack callStack
              ]
      _otherwise ->
        ParseNoMatch

-- | Specialization of 'parseConstriant', just checking the class name
parseConstraint' ::
     HasCallStack
  => Class                        -- ^ Predicate we want to match against
  -> ([Type] -> Maybe a)          -- ^ Parser for the class arguments
  -> Ct                           -- ^ Constraint to parse
  -> ParseResult e (GenLocated CtLoc a)
parseConstraint' cls = parseConstraint p
  where
    p :: Class -> [Type] -> Maybe [Type]
    p cls' args = if cls == cls' then Just args else Nothing

-- | Parse @x ': xs == (':) x xs == ((':) x) xs@
parseCons :: Type -> Maybe (Type, Type)
parseCons t = do
    ( t'  , xs ) <- splitAppTy_maybe t
    ( t'' , x  ) <- splitAppTy_maybe t'
    tcCons <- tyConAppTyCon_maybe t''
    guard $ tcCons == promotedConsDataCon
    return (x, xs)

-- | Parse @'[]@
parseNil :: Type -> Maybe ()
parseNil t = do
    tcNil <- tyConAppTyCon_maybe t
    guard $ tcNil == promotedNilDataCon
    return ()

-- | Parse @'(x, y) == '(,) x y == ('(,) x) y@
parsePair :: Type -> Maybe (Type, Type)
parsePair t = do
    ( t'  , y ) <- splitAppTy_maybe t
    ( t'' , x ) <- splitAppTy_maybe t'
    tcPair <- tyConAppTyCon_maybe t''
    guard $ tcPair == promotedTupleDataCon Boxed 2
    return (x, y)








