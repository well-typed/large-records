{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}

-- | Quasi-quoter support for large records
--
-- NOTE: The only reason for the existence of this module is that record pattern
-- syonyms in @ghc@ are currently not useable: when we declare a record pattern
-- synonym, @ghc@ automatically derives field accessors for every field in the
-- record. We don't want those accessors: they result in name clashes
-- (DuplicateRecordFields does not apply to record pattern synonyms) and, more
-- importantly, they result in quadratic code size again. Once the
-- @NoFieldSelectors@ language extension is merged (probably @ghc@ 9.2), we
-- can reconsider whether this module is still required.
--
-- See also:
--
-- * <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0160-no-toplevel-field-selectors.rst>
-- * <https://gitlab.haskell.org/ghc/ghc/-/merge_requests/4743>
module Data.Record.QQ.CodeGen (
    lr

    -- * Exported for the benefit of tests
  , lrExp
  , lrPat
  ) where

import Data.List (intercalate)
import Data.Maybe (mapMaybe)
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

import qualified Data.Generics         as SYB
import qualified Language.Haskell.Exts as HSE
import qualified Language.Haskell.Meta as HSE.Meta

import Data.Record.QQ.CodeGen.HSE
import Data.Record.QQ.CodeGen.View
import Data.Record.QQ.Runtime.MatchHasField
import Data.Record.TH.CodeGen.Name (FieldName)
import Data.Record.TH.CodeGen.TH
import Data.Record.TH.CodeGen.Tree
import Data.Record.TH.Config.Naming

import qualified Data.Record.TH.CodeGen.Name as N

{-------------------------------------------------------------------------------
  Top-level quasi-quoter
-------------------------------------------------------------------------------}

-- | Construct or match on @large-records@-style records
--
-- Example construction usage:
--
-- > inOrder :: R Bool
-- > inOrder = [lr| MkR { x = 1234, y = [True] } |]
--
-- or:
--
-- > constructorApp :: R Bool
-- > constructorApp = [lr| MkR |] 1234 [True]
--
-- Example matching usage:
--
-- > projectOne :: T Bool -> Int
-- > projectOne [lr| MkT { x = a } |] = a
lr :: QuasiQuoter
lr = QuasiQuoter {
      quoteExp  = lrExp
    , quotePat  = lrPat
    , quoteType = unsupported
    , quoteDec  = unsupported
    }
  where
    unsupported :: String -> Q a
    unsupported _ = fail "lr can only be used for expressions or patterns"

{-------------------------------------------------------------------------------
  Individual quasi-quoters
-------------------------------------------------------------------------------}

lrExp :: forall m. Quasi m => String -> m Exp
lrExp = \str -> do
    exts <- runQ extsEnabled
    case parseExp exts str of
      Left  err  -> fail $ parseErr err
      Right expr -> construct expr
  where
    parseExp :: [Extension] -> String -> Either String Exp
    parseExp exts str =
        case HSE.parseExpWithMode (parseMode exts) str of
          HSE.ParseFailed _loc err -> Left err
          HSE.ParseOk e -> Right (HSE.Meta.toExp e)

    parseErr :: String -> String
    parseErr err = concat [
          "Could not parse expression: "
        , map (\c -> if c == '\n' then ' ' else c) err
        ]

lrPat :: forall m. Quasi m => String -> m Pat
lrPat = \str -> do
    exts <- runQ extsEnabled
    case parsePat exts str of
      Left  err  -> fail $ parseErr err
      Right expr -> deconstruct expr
  where
    parsePat :: [Extension] -> String -> Either String Pat
    parsePat exts str =
        case HSE.parsePatWithMode (parseMode exts) str of
          HSE.ParseFailed _loc err -> Left err
          HSE.ParseOk p -> Right (HSE.Meta.toPat (processRecordPuns p))

    parseErr :: String -> String
    parseErr err = concat [
          "Could not parse pattern: "
        , map (\c -> if c == '\n' then ' ' else c) err
        ]

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

construct :: forall m. Quasi m => Exp -> m Exp
construct = \case
    ConE constr -> do
      constr' <- resolveConstr constr
      runQ $ N.varE $ resolveNameConstructorFn constr'
    expr ->
      -- Assume this is a record construction expression
      SYB.everywhereM (SYB.mkM go) expr
  where
    go :: Exp -> m Exp
    go e = do
        mTerm <- matchRecordExp e
        case mTerm of
          Nothing ->
            -- Leave non-record expressions alone
            return e
          Just NotKnownLargeRecord ->
            return e
          Just (UnknownFields unknown) -> runQ $ do
            reportError $ "Unknown fields: "
                       ++ intercalate ", " (map N.showName unknown)
            [| undefined |]
          Just (MatchedRecord Record{..}) -> runQ $
            appsE $ N.varE (resolveNameConstructorFn recordConstr)
                  : map mkArg recordFields

    mkArg :: Field Exp -> Q Exp
    mkArg Field{..}
      | Just dec <- fieldDec =
          return dec
      | otherwise = do
          reportWarning $ "No value for field " ++ N.showName fieldUnqual
          [| error $ "No value given for field "
                 ++ $(N.termLevelMetadata fieldUnqual) |]

{-------------------------------------------------------------------------------
  Deconstruction
-------------------------------------------------------------------------------}

deconstruct :: forall m. Quasi m => Pat -> m Pat
deconstruct = \pat -> do
    requiresExtensions [TypeApplications, ViewPatterns, DataKinds]
    SYB.everywhereM (SYB.mkM go) pat
  where
    go :: Pat -> m Pat
    go p = do
         mTerm <- matchRecordPat p
         case mTerm of
           Nothing -> -- Not a record pattern
             return p
           Just NotKnownLargeRecord ->
             return p
           Just (UnknownFields unknown) -> runQ $ do
             reportError $ "Unknown fields: "
                        ++ intercalate ", " (map N.showName unknown)
             return p
           Just (MatchedRecord Record{..}) -> runQ $
             viewP (varE 'matchHasField) $
               mkTupleP (uncurry mkPat) $
                 nest (MaxTupleElems 2) (mapMaybe getPat recordFields)

    getPat :: Field Pat -> Maybe (FieldName, Pat)
    getPat Field{..} = (fieldUnqual, ) <$> fieldDec

    mkPat :: FieldName -> Pat -> Q Pat
    mkPat field =
        viewP (varE 'fieldNamed `appTypeE` N.typeLevelMetadata field)
      . return

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

parseMode :: [Extension] -> HSE.ParseMode
parseMode exts = HSE.defaultParseMode {
      HSE.extensions = concat [
          -- Include extensions enabled in the module
          map extensionFromTH exts

          -- But also include the default
          --
          -- We do this primarily because 'fromTH' doesn't actually parse
          -- all extensions
        , HSE.extensions HSE.defaultParseMode
        ]
    }
