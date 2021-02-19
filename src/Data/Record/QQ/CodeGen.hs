{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}

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
module Data.Record.QQ.CodeGen (lr) where

import Data.Maybe (mapMaybe)
import Language.Haskell.TH
import Language.Haskell.TH.Quote

import qualified Data.Generics         as SYB
import qualified Language.Haskell.Exts as HSE
import qualified Language.Haskell.Meta as HSE.Meta

import Data.Record.QQ.CodeGen.HSE
import Data.Record.QQ.CodeGen.View
import Data.Record.QQ.Runtime.MatchHasField
import Data.Record.TH.CodeGen.Name (FieldName)
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
-- Example matching usage:
--
-- > projectOne :: T Bool -> Int
-- > projectOne [lr| MkT { x = a } |] = a
lr :: QuasiQuoter
lr = QuasiQuoter {
      quoteExp  = go "expression" parseExp   construct
    , quotePat  = go "pattern"    parsePat deconstruct
    , quoteType = wrongContext
    , quoteDec  = wrongContext
    }
  where
    wrongContext :: String -> Q a
    wrongContext _ =
        fail "lr can only be used in expression or pattern contexts"

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

    parseExp :: [Extension] -> String -> Either String Exp
    parseExp exts str =
        case HSE.parseExpWithMode (parseMode exts) str of
          HSE.ParseFailed _loc err -> Left err
          HSE.ParseOk e -> Right (HSE.Meta.toExp e)

    parsePat :: [Extension] -> String -> Either String Pat
    parsePat exts str =
        case HSE.parsePatWithMode (parseMode exts) str of
          HSE.ParseFailed _loc err -> Left err
          HSE.ParseOk p -> Right (HSE.Meta.toPat (processRecordPuns p))

    go :: String                                      -- Label
       -> ([Extension] -> String -> Either String a)  -- Parser
       -> (a -> Q a)                                  -- Construction
       -> String -> Q a
    go label f g str = do
        exts <- extsEnabled
        case f exts str of
          Left  err  -> fail (parseErr label err)
          Right expr -> g expr

    parseErr :: String -> String -> String
    parseErr label err = concat [
          "Could not parse "
        , label
        , ": "
        , map (\c -> if c == '\n' then ' ' else c) err
        ]

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

construct :: Exp -> Q Exp
construct =  SYB.everywhereM (SYB.mkM go)
  where
    go :: Exp -> Q Exp
    go e = do
        mTerm <- matchRecordExp e
        case mTerm of
          Nothing ->
            -- Leave non-record expressions alone
            return e
          Just Record{..} ->
            appsE $ N.varE (resolveNameConstructorFn recordConstr)
                  : map mkArg recordFields

    mkArg :: Field Exp -> Q Exp
    mkArg Field{..}
      | Just dec <- fieldDec = return dec
      | otherwise            = [| undefined |]


{-------------------------------------------------------------------------------
  Deconstruction
-------------------------------------------------------------------------------}

deconstruct :: Pat -> Q Pat
deconstruct =  SYB.everywhereM (SYB.mkM go)
  where
    go :: Pat -> Q Pat
    go p = do
         mTerm <- matchRecordPat p
         case mTerm of
           Nothing ->
             return p
           Just Record{..} ->
             viewP (varE 'matchHasField) $
               mkTupleP (uncurry mkPat) $
                 nest (mapMaybe getPat recordFields)

    getPat :: Field Pat -> Maybe (FieldName, Pat)
    getPat Field{..} = (fieldUnqual, ) <$> fieldDec

    mkPat :: FieldName -> Pat -> Q Pat
    mkPat field =
        viewP (varE 'fieldNamed `appTypeE` N.typeLevelMetadata field)
      . return
