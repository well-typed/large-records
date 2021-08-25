{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE DataKinds #-}

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
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

import qualified Data.Generics         as SYB
import qualified Language.Haskell.Exts as HSE
import qualified Language.Haskell.Meta as HSE.Meta

import Data.Record.Internal.CodeGen
import Data.Record.Internal.Record
import Data.Record.Internal.Record.Resolution
import Data.Record.Internal.TH.Util
import Data.Record.QQ.CodeGen.HSE
import Data.Record.QQ.CodeGen.Parser
import Data.Record.QQ.Runtime.Constructor
import Data.Record.QQ.Runtime.MatchHasField
import Data.Record.TH.CodeGen.Tree
import Data.Record.TH.Config.Options (GenPatSynonym(..))

import qualified Data.Record.Internal.TH.Name as N

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
    ConE (fromHseName -> constr) -> do
      mr <- resolveRecord constr
      runQ $ case mr of
               Left  _ -> fail $ "Not in scope: " ++ show (N.nameBase constr)
               Right r -> resolveRecordConstr (N.nameQualifier constr) r
    expr ->
      -- Assume this is a record construction expression
      SYB.everywhereM (SYB.mkM go) expr
  where
    go :: Exp -> m Exp
    go e = do
        mTerm <- parseRecordExp e
        case mTerm of
          Nothing ->
            -- Leave non-record expressions alone
            return e
          Just NotKnownLargeRecord ->
            return e
          Just (UnknownFields unknown) -> runQ $ do
            reportError $ "Unknown fields: " ++ intercalate ", " unknown
            [| undefined |]
          Just (ParsedRecordInfo qual r@Record{..}) -> runQ $ do
            appsE $ resolveRecordConstr qual r
                  : map mkArg recordFields

    mkArg :: Field (Maybe Exp) -> Q Exp
    mkArg Field{..}
      | Just e <- fieldVal = return e
      | otherwise = do
          reportWarning $ "No value for field " ++ fieldName
          [| error $ "No value given for field " ++ $(lift fieldName) |]

resolveRecordConstr :: N.Qualifier -> Record a -> Q Exp
resolveRecordConstr qual r = [|
      __recordConstructor $(recordUndefinedValueE UseQuasiQuoter qual r)
    |]

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
         mTerm <- parseRecordPat p
         case mTerm of
           Nothing -> -- Not a record pattern
             return p
           Just NotKnownLargeRecord ->
             return p
           Just (UnknownFields unknown) -> runQ $ do
             reportError $ "Unknown fields: " ++ intercalate ", " unknown
             return p
           Just (ParsedRecordInfo qual r) -> runQ $
             viewP (   varE 'viewAtType
                     `appE`
                       recordUndefinedValueE UseQuasiQuoter qual r
                   ) $
               case recordFields (dropMissingRecordFields r) of
                 [] -> bangP wildP
                 fs -> outerViewPat fs

    outerViewPat :: [Field Pat] -> Q Pat
    outerViewPat fs =
        viewP (varE 'matchHasField) $
          mkTupleP innerViewPat $ nest (MaxTupleElems 2) fs

    innerViewPat :: Field Pat -> Q Pat
    innerViewPat f@Field{..} =
        viewP
          (varE 'fieldNamed `appTypeE` fieldNameT f)
          (return fieldVal)

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
