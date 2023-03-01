{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns    #-}
{-# LANGUAGE NamedFieldPuns #-}

module Data.Record.Anon.Internal.Plugin.TC.Rewriter (rewrite) where

import Data.Record.Anon.Internal.Plugin.TC.Row.KnownRow (KnownRow)
import Data.Record.Anon.Internal.Plugin.TC.Row.ParsedRow (Fields)
import Data.Record.Anon.Internal.Plugin.TC.GhcTcPluginAPI
import Data.Record.Anon.Internal.Plugin.TC.NameResolution
import Data.Record.Anon.Internal.Plugin.TC.TyConSubst

import qualified Data.Record.Anon.Internal.Plugin.TC.Row.KnownField as KnownField
import qualified Data.Record.Anon.Internal.Plugin.TC.Row.KnownRow   as KnownRow
import qualified Data.Record.Anon.Internal.Plugin.TC.Row.ParsedRow  as ParsedRow

rewrite :: ResolvedNames -> UniqFM TyCon TcPluginRewriter
rewrite rn@ResolvedNames{..} = listToUFM [
      (tyConFieldTypes       , rewriteRecordMetadataOf tyConFieldTypes       rn)
    , (tyConSimpleFieldTypes , rewriteRecordMetadataOf tyConSimpleFieldTypes rn)
    ]

data Args = Args {
      -- | Functor argument, if any
      argsFunctor :: Maybe Type

      -- | Parsed fields
    , argsParsedFields :: Maybe Fields

      -- | Known record, if all fields are known
    , argsParsedKnown :: Maybe (KnownRow Type)
    }

mkArgs :: TyConSubst -> ResolvedNames -> Maybe Type -> Type -> Args
mkArgs tcs rn argsFunctor r = Args{..}
  where
    argsParsedFields = ParsedRow.parseFields tcs rn r
    argsParsedKnown  = ParsedRow.allKnown =<< argsParsedFields

parseArgs :: [Ct] -> ResolvedNames -> [Type] -> Args
parseArgs given rn = \case
    [_k, f, r] -> mkArgs tcs rn (Just f) r
    [       r] -> mkArgs tcs rn Nothing  r
    args       -> panic $ concat [
        "Data.Record.Anon.Plugin.Rewriter.parseArgs: "
      , "unexpected arguments: "
      , showSDocUnsafe (ppr args)
      ]
  where
    tcs :: TyConSubst
    tcs = mkTyConSubst given

rewriteRecordMetadataOf :: TyCon -> ResolvedNames -> TcPluginRewriter
rewriteRecordMetadataOf fun rn given args@(parseArgs given rn -> Args{..}) =
--  trace _debugInput  $
--  trace _debugParsed $
    case argsParsedKnown of
      Nothing ->
        return TcPluginNoRewrite
      Just knownFields ->
        return TcPluginRewriteTo {
            tcRewriterNewWanteds = []
          , tcPluginReduction    =
               mkTyFamAppReduction
                 "large-anon"
                 Nominal
                 fun
                 args
                 (computeMetadataOf argsFunctor knownFields)
          }
  where
    _debugInput :: String
    _debugInput = unlines [
          "*** input"
        , concat [
              "given:"
            , showSDocUnsafe (ppr given)
            ]
        , concat [
              "args: "
            , showSDocUnsafe (ppr args)
            ]
        ]

    _debugParsed :: String
    _debugParsed = unlines [
          "*** parsed"
        , concat [
              "parsedFields: "
            , showSDocUnsafe (ppr argsParsedFields)
            ]
        , concat [
              "mKnownFields: "
            , showSDocUnsafe (ppr argsParsedKnown)
            ]
        ]

computeMetadataOf :: Maybe Type -> KnownRow Type -> TcType
computeMetadataOf mf r =
    mkPromotedListTy
      (mkTupleTy Boxed [mkTyConTy typeSymbolKindCon, liftedTypeKind])
      (map (KnownField.toType mf) $ KnownRow.toList r)
