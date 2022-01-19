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
  , parseInjTyConApp
  ) where

import Data.Bifunctor
import Data.Foldable (toList)
import Data.Void
import GHC.Stack

import Data.Record.Anonymous.Plugin.GhcTcPluginAPI
import Data.Record.Anonymous.Plugin.TyConSubst

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
    -- TODO: classify up to equalities..?
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
parseCons :: TyConSubst -> Type -> Maybe (Type, Type)
parseCons tcs t = do
    args <- parseInjTyConApp tcs promotedConsDataCon t
    case args of
      [_k, x, xs] -> Just (x, xs)
      _otherwise  -> Nothing

-- | Parse @'[]@
parseNil :: TyConSubst -> Type -> Maybe ()
parseNil tcs t = do
    args <- parseInjTyConApp tcs promotedNilDataCon t
    case args of
      [_k]       -> Just ()
      _otherwise -> Nothing

-- | Parse @'(x, y) == '(,) x y == ('(,) x) y@
parsePair :: TyConSubst -> Type -> Maybe (Type, Type)
parsePair tcs t = do
    args <- parseInjTyConApp tcs (promotedTupleDataCon Boxed 2) t
    case args of
      [_kx, _ky, x, y] -> Just (x, y)
      _otherwise       -> Nothing

-- | Parse application of an injective type constructor
parseInjTyConApp :: TyConSubst -> TyCon -> Type -> Maybe [Type]
parseInjTyConApp tcs tyCon t = do
    splits <- splitTyConApp_upTo tcs t

    -- At this point we might have multiple matches
    --
    -- > t ~ TyCon1 args1
    -- > t ~ TyCon1 args1'
    -- > t ~ TyCon2 args2
    -- > ..
    --
    -- We are only interested in the equalities with @tyCon@ at the head, but
    -- this may still leave us with multiple equalities
    --
    -- > t ~ tyCon args1
    -- > t ~ tyCon args1'
    --
    -- When this is the case, however, by injectivity of 'tyCon' we know that
    -- @args1 ~ args1'@, so we can just return /any/ of the matches; we will
    -- return the first.
    lookup tyCon (toList splits)
