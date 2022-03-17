{-# LANGUAGE RecordWildCards #-}

module Data.Record.Anonymous.Plugin.Rewriter (rewrite) where

import Data.Record.Anonymous.Plugin.GhcTcPluginAPI
import Data.Record.Anonymous.Plugin.NameResolution
import Data.Record.Anonymous.Plugin.Record
import Data.Record.Anonymous.Plugin.TyConSubst

import qualified Data.Record.Anonymous.Internal.FieldName as FieldName

rewrite :: ResolvedNames -> UniqFM TyCon TcPluginRewriter
rewrite rn@ResolvedNames{..} = listToUFM [
      (tyConFieldTypes, rewriteRecordMetadataOf rn)
    ]

rewriteRecordMetadataOf :: ResolvedNames -> TcPluginRewriter
rewriteRecordMetadataOf rn@ResolvedNames{..} given args@[functor, fields] =
--  trace _debugInput  $
--  trace _debugParsed $
    case mKnownFields of
      Nothing ->
        return TcPluginNoRewrite
      Just knownFields ->
        return TcPluginRewriteTo {
            tcRewriterWanteds = []
          , tcPluginReduction =
               mkTyFamAppReduction
                 "large-anon"
                 Nominal
                 tyConFieldTypes
                 args
                 (computeMetadataOf functor knownFields)
          }
  where
    tcs :: TyConSubst
    tcs = mkTyConSubst given

    parsedFields :: Maybe Fields
    parsedFields = parseFields tcs rn fields

    mKnownFields :: Maybe (KnownRecord ())
    mKnownFields = checkAllFieldsKnown =<< parsedFields

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
            , showSDocUnsafe (ppr parsedFields)
            ]
        , concat [
              "mKnownFields: "
            , showSDocUnsafe (ppr mKnownFields)
            ]
        ]

rewriteRecordMetadataOf _rn _given _args =
    panic $ "rewriteRecordMetadataOf: unexpected arguments"

computeMetadataOf :: Type -> KnownRecord () -> TcType
computeMetadataOf functor fields =
    mkPromotedListTy
      (mkTupleTy Boxed [mkTyConTy typeSymbolKindCon, liftedTypeKind])
      (map aux $ knownRecordFields fields)
  where
    aux :: KnownField () -> Type
    aux KnownField{..} =
        -- mkPromotedPairTy is only introduced in ghc 9.2
        mkTyConApp
          (promotedTupleDataCon Boxed 2)
          [ mkTyConTy typeSymbolKindCon -- kind of first arg
          , liftedTypeKind              -- kind of second arg
          , FieldName.mkType knownFieldName
          , functor `mkAppTy` knownFieldType
          ]
