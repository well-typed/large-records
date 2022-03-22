{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns    #-}

module Data.Record.Anonymous.Plugin.Rewriter (rewrite) where

import Data.Record.Anonymous.Plugin.GhcTcPluginAPI
import Data.Record.Anonymous.Plugin.NameResolution
import Data.Record.Anonymous.Plugin.Record
import Data.Record.Anonymous.Plugin.TyConSubst

import qualified Data.Record.Anonymous.Internal.FieldName as FieldName

rewrite :: ResolvedNames -> UniqFM TyCon TcPluginRewriter
rewrite rn@ResolvedNames{..} = listToUFM [
      (tyConFieldTypes       , rewriteRecordMetadataOf rn)
    , (tyConSimpleFieldTypes , rewriteRecordMetadataOf rn)
    ]

data Args = Args {
      -- | Functor argument, if any
      argsFunctor :: Maybe Type

      -- | Parsed fields
    , argsParsedFields :: Maybe Fields

      -- | Known record, if all fields are known
    , argsParsedKnown :: Maybe (KnownRecord ())
    }

mkArgs :: TyConSubst -> ResolvedNames -> Maybe Type -> Type -> Args
mkArgs tcs rn argsFunctor r = Args{..}
  where
    argsParsedFields = parseFields tcs rn r
    argsParsedKnown  = checkAllFieldsKnown =<< argsParsedFields

parseArgs :: [Ct] -> ResolvedNames -> [Type] -> Args
parseArgs given rn = \case
    [_k, f, r] -> mkArgs tcs rn (Just f) r
    [       r] -> mkArgs tcs rn Nothing  r
    args       -> panic $ concat [
        "Data.Record.Anonymous.Plugin.Rewriter.parseArgs: "
      , "unexpected arguments: "
      , showSDocUnsafe (ppr args)
      ]
  where
    tcs :: TyConSubst
    tcs = mkTyConSubst given

rewriteRecordMetadataOf :: ResolvedNames -> TcPluginRewriter
rewriteRecordMetadataOf rn@ResolvedNames{..} given args@(parseArgs given rn -> Args{..}) =
--  trace _debugInput  $
--  trace _debugParsed $
    case argsParsedKnown of
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

computeMetadataOf :: Maybe Type -> KnownRecord () -> TcType
computeMetadataOf mf r =
    mkPromotedListTy
      (mkTupleTy Boxed [mkTyConTy typeSymbolKindCon, liftedTypeKind])
      (map aux $ knownRecordFields r)
  where
    aux :: KnownField () -> Type
    aux KnownField{..} =
        -- mkPromotedPairTy is only introduced in ghc 9.2
        mkTyConApp
          (promotedTupleDataCon Boxed 2)
          [ mkTyConTy typeSymbolKindCon -- kind of first arg
          , liftedTypeKind              -- kind of second arg
          , FieldName.mkType knownFieldName
          , case mf of
              Just f  -> f `mkAppTy` knownFieldType
              Nothing -> knownFieldType
          ]
