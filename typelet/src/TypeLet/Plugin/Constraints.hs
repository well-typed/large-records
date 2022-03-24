module TypeLet.Plugin.Constraints (
    -- * Constraints recognized by the plugin
    CEqual(..)
  , CLet(..)
    -- * Parsing
    -- ** Infrastructure
  , ParseResult(..)
  , parseAll
  , parseAll'
  , withOrig
    -- ** SPecific parsers
  , InvalidLet(..)
  , parseEqual
  , parseLet
    -- * Evidence construction
  , evidenceEqual
    -- * Formatting errors
  , formatCLet
  , formatInvalidLet
  ) where

import Data.Bifunctor
import Data.Void

import TypeLet.Plugin.GhcTcPluginAPI
import TypeLet.Plugin.NameResolution

{-------------------------------------------------------------------------------
  Constraints recognized by the plugin
-------------------------------------------------------------------------------}

data CLet = CLet {
      letKind :: Type
    , letLHS  :: TyVar
    , letRHS  :: Type
    }

data CEqual = CEqual {
      equalKind :: Type
    , equalLHS  :: Type
    , equalRHS  :: Type
    }

instance Outputable CLet where
  ppr (CLet k a b) = parens $ text "Let" <+> ppr k <+> ppr a <+> ppr b

instance Outputable CEqual where
  ppr (CEqual k a b) = parens $ text "Equal" <+> ppr k <+> ppr a <+> ppr b

{-------------------------------------------------------------------------------
  Parsing infrastructure
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
  Parser for specific constraints

  We can assume here that the constraint is kind correct, so if the class
  matches, we know how many arguments
-------------------------------------------------------------------------------}

data InvalidLet =
    -- | LHS should always be a variable
    NonVariableLHS Type Type Type

    -- | The LHS should be a /skolem/ variable
    --
    -- As for as ghc is concerned, the LHS should be an opaque type variable
    -- with unknown value (only the plugin knows); certainly, ghc should not
    -- try to unify it with anything.
  | NonSkolemLHS Type TyVar Type

parseLet ::
     ResolvedNames
  -> Ct
  -> ParseResult (GenLocated CtLoc InvalidLet) (GenLocated CtLoc CLet)
parseLet ResolvedNames{..} ct = bimap (L $ ctLoc ct) (L $ ctLoc ct) $
    case classifyPredType (ctPred ct) of
      ClassPred cls [k, a, b] | cls == clsLet ->
        case getTyVar_maybe a of
          Nothing ->
            ParseError $ NonVariableLHS k a b
          Just x  ->
            if isSkolemTyVar x
              then ParseOk    $ CLet         k x b
              else ParseError $ NonSkolemLHS k x b
      _otherwise ->
        ParseNoMatch

-- | Parse 'Equal' constraints
--
-- Kind-correct 'Equal' constraints of any form are ok, so this cannot return
-- errors.
parseEqual :: ResolvedNames -> Ct -> ParseResult Void (GenLocated CtLoc CEqual)
parseEqual ResolvedNames{..} ct = fmap (L $ ctLoc ct) $
    case classifyPredType (ctPred ct) of
      ClassPred cls [k, a, b] | cls == clsEqual ->
        ParseOk $ CEqual k a b
      _otherwise ->
        ParseNoMatch

{-------------------------------------------------------------------------------
  Evidence construction
-------------------------------------------------------------------------------}

-- | Evidence for an 'Equal' constraint
--
-- TODO: should we worry about producing an evidence term that prevents floating
-- stuff out of scope...? (the whole "coercions cannot simply be zapped" thing)
-- See also https://gitlab.haskell.org/ghc/ghc/-/issues/8095#note_108189 .
evidenceEqual :: ResolvedNames -> CEqual -> EvTerm
evidenceEqual ResolvedNames{..} (CEqual k a b) =
    evDataConApp
      (classDataCon clsEqual)
      [k, a, b]
      []

{-------------------------------------------------------------------------------
  Formatting errors
-------------------------------------------------------------------------------}

formatCLet :: CLet -> TcPluginErrorMessage
formatCLet (CLet _ a b) =
        PrintType (mkTyVarTy a)
    :|: Txt " := "
    :|: PrintType b

formatInvalidLet :: InvalidLet -> TcPluginErrorMessage
formatInvalidLet (NonVariableLHS _k a b) =
        Txt "Let with non-variable LHS: "
    :|: PrintType a :|: Txt " := " :|: PrintType b
formatInvalidLet (NonSkolemLHS _k a b) =
        Txt "Let with non-skolem LHS: "
    :|: PrintType (mkTyVarTy a) :|: Txt " := " :|: PrintType b