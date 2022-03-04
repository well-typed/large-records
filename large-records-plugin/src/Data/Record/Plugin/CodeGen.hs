{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ParallelListComp #-}

-- | The core of the plugin implementation.
--
-- TODO: This is the equivalent of Data.Record.TH.CodeGen. We should make sure
-- the documentation of that module is moved here.
module Data.Record.Plugin.CodeGen (genLargeRecord) where

import Data.List (nubBy)

import qualified Data.Generics as SYB

import GhcPlugins (noLoc)

import Data.Record.Plugin.GHC.TemplateHaskellStyle
import Data.Record.Plugin.RuntimeNames as Runtime
import Data.Record.Plugin.Types.Options (shouldGeneratedHasField, shouldRecordBeStrict)
import Data.Record.Plugin.Types.Record (Record (..), RecordDeriving (..), StockDeriving (..))

recordName :: Record -> String
recordName Record {tyName} = nameBase tyName

nameVectorFrom :: Record -> RdrName
nameVectorFrom rec = ExpVar ("vectorFrom" <> recordName rec)

nameVectorTo :: Record -> RdrName
nameVectorTo rec = ExpVar ("vectorTo" <> recordName rec)

nameUnsafeGetIndex :: Record -> RdrName
nameUnsafeGetIndex rec = ExpVar ("unsafeGetIndex" <> recordName rec)

nameUnsafeSetIndex :: Record -> RdrName
nameUnsafeSetIndex rec = ExpVar ("unsafeSetIndex" <> recordName rec)

nameConstraints :: Record -> RdrName
nameConstraints rec = TyCon ("Constraints_" <> recordName rec)

nameDictConstraints :: Record -> RdrName
nameDictConstraints rec = ExpVar ("dictConstraints_" <> recordName rec)

-- | Generate all large-records definitions for a record.
genLargeRecord :: Record -> [LHsDecl GhcPs]
genLargeRecord rec@Record {fields, options} =
  concat
    [ [genDatatype rec],
      genVectorFrom rec,
      genVectorTo rec,
      genUnsafeGetIndex rec,
      genUnsafeSetIndex rec,
      if shouldGeneratedHasField options
        then [genHasFieldInstance rec i f | i <- [0 ..] | f <- fields]
        else [],
      [ genConstraintsClass rec,
        genConstraintsInstance rec,
        genGenericInstance rec,
        genGHCGeneric rec
      ],
      genStockInstances rec
    ]

-- | Generate a type expression @R t1 t2 ...@ (for whatever type variables are present in the definition of the record).
genRecordTy :: Record -> LHsType GhcPs
genRecordTy Record {tyName, tyVars} = VarT tyName `appsT` [VarT (tyVarBndrName f) | f <- tyVars]

-- | Generate a large-records representation for record @T@.
--
-- If the original record looks like
--
-- > data T a = T {x :: a, y :: String}
--
-- the generated representation will look like this:
--
-- > data T a = forall tx ty. (tx ~ a, ty ~ String) => T {x :: tx, y :: ty}
-- >   deriving anyclass C -- where applicable
--
-- (possibly with strict fields). This representation accomplishes two things:
--
-- 1. The use of the existentials with type equalities prevents ghc from
--    generating field accessors.
-- 2. It can still be used in the normal way to construct record values and
--    to pattern match on records.
--
-- TODO: From ghc 9.2 and up, we should generate
--
-- > newtype T a b = TFromVector {vectorFromT :: Vector Any}
-- >   deriving anyclass C -- where applicable
--
-- instead, along with a pattern synonym.
--
-- TODO: Do we want to support anyclass deriving...?
-- See discussion in <https://github.com/well-typed/large-records/pull/42>.
genDatatype :: Record -> LHsDecl GhcPs
genDatatype Record {tyName, conName, tyVars, fields, derivings, options} =
    DataD
      tyName
      tyVars
      [ forallRecC
          vars
          (zipWith fieldContext vars fields)
          conName
          (zipWith fieldType vars fields)

      ]
      [ DerivClause (Just (noLoc AnyclassStrategy)) [c]
      | DeriveAnyClass c <- derivings
      ]
  where
    vars :: [RdrName]
    vars = [TyVar ("lr_f" <> show i) | (i, _) <- zip [1 :: Int ..] fields]

    optionalBang :: LHsType GhcPs -> LHsType GhcPs
    optionalBang = if shouldRecordBeStrict options then bangType else id

    fieldContext :: RdrName -> (RdrName, LHsType GhcPs) -> LHsType GhcPs
    fieldContext var (_name, typ) = equalP (VarT var) typ

    fieldType :: RdrName -> (RdrName, LHsType GhcPs) -> (RdrName, LHsType GhcPs)
    fieldType var (name, _typ) = (name, optionalBang $ VarT var)

-- | Generate @vectorFromT@ for record @T@.
--
-- > vectorFromT :: T ... -> Vector Any
-- > vectorFromT = \(T field1 ...) -> V.fromList [unsafeCoerce field1, ...]
--
-- TODO: From GHC 9.2, this could be an identify function after changing the record representation.
genVectorFrom :: Record -> [LHsDecl GhcPs]
genVectorFrom rec@Record {conName, fields} =
  let body =
        lamE
          [conP conName [varP f | (f, _) <- fields]]
          (VarE Runtime.fromList `appE` listE [VarE Runtime.unsafeCoerce `appE` VarE f | (f, _) <- fields])
      name = (nameVectorFrom rec)
   in [ sigD name (genRecordTy rec `funT` (VarT Runtime.type_Vector `appT` VarT Runtime.type_Any))
      , valD name body
      ]

-- | Generate @vectorToT@ for record @T@.
--
-- > vectorToT :: Vector Any -> T ...
-- > vectorToT = \x -> case toList x of
-- >     [field1, ...] -> T (unsafeCoerce field1) ...
-- >     _ -> error "Pattern match failure in vectorToT: vector with invalid number of elements."
--
-- TODO: From GHC 9.2, this could be an identify function after changing the record representation.
genVectorTo :: Record -> [LHsDecl GhcPs]
genVectorTo rec@Record {conName, fields} =
  let body =
        lamE [varP nameArg] do
          caseE
            (VarE Runtime.toList `appE` VarE nameArg)
            [ (listP [varP f | (f, _) <- fields], appsE (VarE conName) [VarE Runtime.unsafeCoerce `appE` VarE f | (f, _) <- fields]),
              (wildP, VarE Runtime.error `appE` stringE matchErr)
            ]
      name = nameVectorTo rec
   in [ sigD name ((VarT Runtime.type_Vector `appT` VarT Runtime.type_Any) `funT` genRecordTy rec)
      , valD name body
      ]
  where
    nameArg = ExpVar "x"
    matchErr =
      concat
        [ "Pattern match failure in ",
          nameBase (nameVectorTo rec),
          ": vector with invalid number of elements."
        ]

-- | Generate @unsafeGetIndexT@ for record @T@.
--
-- > unsafeGetIndexT :: Int -> T ... -> val
-- > unsafeGetIndexT = \index arg -> noInlineUnsafeCo (V.unsafeIndex (Main.vectorFromB arg) index)
genUnsafeGetIndex :: Record -> [LHsDecl GhcPs]
genUnsafeGetIndex rec = [
      sigD name (VarT Runtime.type_Int `funT` (genRecordTy rec `funT` VarT (TyVar "lr_result_")))
    , valD name ( lamE
                    [varP index, varP arg]
                    (VarE Runtime.noInlineUnsafeCo `appE` VarE Runtime.unsafeIndex `appsE` [VarE (nameVectorFrom rec) `appE` VarE arg, VarE index])
                )
    ]
  where
    name = nameUnsafeGetIndex rec
    index = ExpVar "index"
    arg = ExpVar "arg"

-- | Generate @unsafeSetIndexT@ for record @T@.
--
-- > unsafeSetIndexT :: Int -> T ... -> val -> T ...
-- > unsafeSetIndexT = \index arg !val ->
-- >   vectorToB (V.unsafeUpd (vectorFromT arg) [(index, noInlineUnsafeCo val)])
--
-- TODO: unlike the Template Haskell version, does not care about @allFieldsStrict@ and is always strict in @val@.
--
-- TODO: We should support per-field strictness.
genUnsafeSetIndex :: Record -> [LHsDecl GhcPs]
genUnsafeSetIndex rec = [
      sigD name (VarT Runtime.type_Int `funT` (genRecordTy rec `funT` (VarT (TyVar "lr_result_") `funT` genRecordTy rec)))
    , valD name ( lamE
                    [varP index, varP arg, bangP (varP val)]
                    ( VarE (nameVectorTo rec)
                        `appE` ( VarE Runtime.unsafeUpd
                                   `appsE` [ VarE (nameVectorFrom rec) `appE` VarE arg,
                                             listE [tupE [VarE index, VarE Runtime.noInlineUnsafeCo `appE` VarE val]]
                                           ]
                               )
                    )
                )
    ]
  where
    name = nameUnsafeSetIndex rec
    index = ExpVar "index"
    arg = ExpVar "arg"
    val = ExpVar "val"

-- | Generate a @HasField@ instance for the @i@-th field of the record with given name and type.
--
-- @unsafeGetIndexT@ and @unsafeSetIndexT@ are generated per-record by 'genUnsafeGetIndex' and 'genUnsafeSetIndex' respectively.
--
-- > instance t ~ fieldType => HasField "fieldName" (T ...) t where
-- >   hasField = \arg -> (unsafeSetIndexT i arg, unsafeGetIndexT i arg)
genHasFieldInstance :: Record -> Int -> (RdrName, LHsType GhcPs) -> LHsDecl GhcPs
genHasFieldInstance rec index (fieldName, fieldTy) =
  instanceD
    [equalP (VarT fieldTyVar) fieldTy]
    (VarT Runtime.type_HasField `appsT` [stringT fieldStr, genRecordTy rec, VarT fieldTyVar])
    [ ( ExpVar "hasField"
      , lamE
          [varP arg]
          ( tupE
              [ appsE (VarE (nameUnsafeSetIndex rec)) [intE index, VarE arg],
                appsE (VarE (nameUnsafeGetIndex rec)) [intE index, VarE arg]
              ]
          )
      )
    ]
    []
  where
    fieldStr = nameBase fieldName
    arg = ExpVar "arg"
    fieldTyVar = TyVar "lr_field_ty"

-- | Generate a @Constraints_T@ class definition for record @T@.
--
-- > class Constraints_T t1 t2 ... (c :: Type -> Constraint) where
-- >   dictConstraints_T :: Proxy c -> Rep (Dict c) (R t1 t2 ...)
--
-- NOTE: It is critical that we don't give the class any superclass constraints
-- like
--
-- > class (c t1, c t2, ...) => Constraints_T t1 t2 ... (c :: Type -> Constraint)
--
-- because then GHC would use resolve @Constraints_T@ to that tuple instead,
-- and use lots of "tuple constraint extractor" functions, each of which have
-- the same size as the number of constraints (another example of a
-- @case f of { T x1 x2 x3 .. -> xn }@ function, but now at the dictionary level).
genConstraintsClass :: Record -> LHsDecl GhcPs
genConstraintsClass rec@Record {tyVars} =
    classD
      []
      (nameConstraints rec)
      (tyVars ++ [kindedTV c (VarT Runtime.type_Type `funT` VarT Runtime.type_Constraint)])
      [ ( nameDictConstraints rec
        , (VarT Runtime.type_Proxy `appT` VarT c) `funT` (VarT Runtime.type_Rep `appsT` [VarT Runtime.type_Dict `appT` VarT c, genRecordTy rec])
        )
      ]
  where
    c = TyVar "lr_con_c"

-- | Generate a @Constraints_T@ instance for record @T@.
--
-- > instance (c field1, c field2, ...) => Constraints_T t1 t2 ... c where
-- >   dictConstraints_T = \p ->
-- >     Rep (V.fromList [ noInlineUnsafeCo (dictFor p (Proxy :: Proxy field1))
-- >                     , noInlineUnsafeCo (dictFor p (Proxy :: Proxy field2))
-- >                     , ... ])
--
-- In the example above, @t1 t2 ...@ refer to type variables in the record definition.
genConstraintsInstance :: Record -> LHsDecl GhcPs
genConstraintsInstance rec@Record {tyVars, fields} =
  instanceD
    [VarT c `appT` ty | (_, ty) <- fields]
    (appsT (VarT (nameConstraints rec)) ([VarT (tyVarBndrName v) | v <- tyVars] ++ [VarT c]))
    [(nameDictConstraints rec, body)]
    []
  where
    c = TyVar "lr_con_c"
    p = ExpVar "p"
    body = lamE [varP p] (VarE Runtime.con_Rep `appE` (VarE Runtime.fromList `appE` listE dicts))
    dicts = [mkDict ty | (_, ty) <- fields]

    mkDict :: LHsType GhcPs -> LHsExpr GhcPs
    mkDict ty = VarE Runtime.noInlineUnsafeCo `appE` (VarE Runtime.dictFor `appsE` [VarE p, genProxy ty])

-- | Generate an instance of large-records 'Data.Record.Generic'.
--
-- In the sample instance below, @vectorFromT@ and @vectorToT@ are generated per-record by 'genVectorFrom' and 'genVectorTo' respectively.
--
-- > instance Generic (T ...) where
-- >   type Constraints (T ...) = Constraints_T ...
-- >   type MetadataOf (T ...) = '[ '("field1", fieldType1), ...]
-- >   from = \x -> repFromVector (vectorFromT x)
-- >   to = \x -> (\y -> rnfVectorAny y `seq` vectorToT y) (repToVector x)
-- >   dict = dictConstraints_T
-- >   metadata = \x -> Metadata
-- >     { recordName = "T"
-- >     , recordConstructor = "T"
-- >     , recordSize = size  -- number of fields
-- >     , recordFieldMetadata = Rep (V.fromList [FieldMetadata (Proxy :: Proxy "field1") FieldStrict, ...])
-- >     }
--
-- TODO: unlike the Template Haskell version, this one does not omit the @rnfVectorAny@ call if all fields are already strict.
genGenericInstance :: Record -> LHsDecl GhcPs
genGenericInstance rec@Record {tyVars, conName, fields} =
  instanceD
    []
    (VarT Runtime.type_Generic `appT` genRecordTy rec)
    [ ( Runtime.from_unqual
      , lamE [varP x] (VarE Runtime.repFromVector `appE` (VarE (nameVectorFrom rec) `appE` VarE x))
      )
    , ( Runtime.to_unqual
      , lamE
            [varP x]
            ( appE
                (lamE [varP y] (VarE Runtime.seq `appsE` [VarE Runtime.rnfVectorAny `appE` VarE y, VarE (nameVectorTo rec) `appE` VarE y]))
                (VarE Runtime.repToVector `appE` VarE x)
            )
      )
    , ( Runtime.dict_unqual
      , VarE (nameDictConstraints rec)
      )
    , ( Runtime.metadata_unqual
      , lamE [varP x] $
          recConE
            Runtime.con_Metadata
            [ (Runtime.field_recordName, stringE (recordName rec)),
              (Runtime.field_recordConstructor, stringE (nameBase conName)),
              (Runtime.field_recordSize, intE (length fields)),
              (Runtime.field_recordFieldMetadata, (VarE Runtime.con_Rep `appE` (VarE Runtime.fromList `appE` metadata)))
            ]
      )
    ]
    [ tySynEqn Runtime.type_Constraints_unqual [genRecordTy rec] constraints,
      tySynEqn Runtime.type_MetadataOf_unqual [genRecordTy rec] metadataOf
    ]
  where
    (x, y) = (ExpVar "x", ExpVar "y")

    constraints = appsT (VarT (nameConstraints rec)) [VarT (tyVarBndrName v) | v <- tyVars]
    metadataOf = listT [tupT [stringT (nameBase f), t] | (f, t) <- fields]
    metadata = listE [mkFieldMD name | (name, _) <- fields]
    mkFieldMD name =
      (appsE (VarE Runtime.con_FieldMetadata))
        [ genProxy (stringT (nameBase name)),
          VarE Runtime.con_FieldStrict
        ]

-- | Generate an instance of @GHC.Generics.Generic@ that goes through large-records generics.
--
-- > instance Generic (T ...) where
-- >   type Rep (T ...) = ThroughLRGenerics (T ...)
-- >   from = WrapThroughLRGenerics
-- >   to = unwrapThroughLRGenerics
genGHCGeneric :: Record -> LHsDecl GhcPs
genGHCGeneric rec =
  (instanceD
    []
    (VarT Runtime.type_GHC_Generic `appT` genRecordTy rec))
    [ (Runtime._GHC_from_unqual, VarE Runtime.con_WrapThroughLRGenerics),
      (Runtime._GHC_to_unqual, VarE Runtime.unwrapThroughLRGenerics)
    ]
    [ tySynEqn Runtime.type_GHC_Rep_unqual [genRecordTy rec] (VarT Runtime.type_ThroughLRGenerics `appT` genRecordTy rec)
    ]

-- | @genRequiredConstraints r c@ generates a list of constraints @[c t1, c t2, ...]@ for all type variables in the definition of @r@.
--
-- TODO: @genRequiredConstraints@ in the Template Haskell version is smarter and generates e.g. @c [b]@ instead of @c b@. This might matter for
-- phantom type variables.
genRequiredConstraints :: Record -> LHsType GhcPs -> [LHsType GhcPs]
genRequiredConstraints Record {fields} c =
    nubBy sameType $ filter hasTypeVar $ map (uncurry constrainField) fields
  where
    constrainField :: RdrName -> LHsType GhcPs -> LHsType GhcPs
    constrainField _ t = c `appT` t

    -- TODO. Not sure what the best way to do this is.
    -- SYB.geq does not work! (Bogus SYB instance for 'Name'.)
    sameType :: LHsType GhcPs -> LHsType GhcPs -> Bool
    sameType _a _b = False

    hasTypeVar :: LHsType GhcPs -> Bool
    hasTypeVar = not . null . allTyVars

    allTyVars :: LHsType GhcPs -> [String]
    allTyVars = SYB.everything (++) (SYB.mkQ [] isTypeVar)

    isTypeVar :: LHsType GhcPs -> [String]
    isTypeVar (VarT (TyVar name)) = [name]
    isTypeVar _otherwise          = []


-- | Generate stock instances for the classes listes in the record's 'derivings' field.
genStockInstances :: Record -> [LHsDecl GhcPs]
genStockInstances rec@Record {derivings} = do
  DeriveStock d <- derivings
  case genStockInstance rec d of
    Just decl -> pure decl
    Nothing -> []

-- | For a record type @T@ and stock-derivable class @C@, generate
--
-- > instance $(genRequiredConstraints T C) => C T where
-- >   $(method) = $(generic implementation)
--
-- Note: doesn't do anything for 'Generic' because we want to generate a custom instance instead. See 'genGHCGeneric'.
genStockInstance :: Record -> StockDeriving -> Maybe (LHsDecl GhcPs)
genStockInstance rec = \case
  Show -> Just (mkInstance Runtime.type_Show Runtime.showsPrec (VarE Runtime.gshowsPrec))
  Eq -> Just (mkInstance Runtime.type_Eq Runtime.eq (VarE Runtime.geq))
  Ord -> Just (mkInstance Runtime.type_Ord Runtime.compare (VarE Runtime.gcompare))
  Generic -> Nothing
  where
    mkInstance cls mthd gen =
      instanceD
        (genRequiredConstraints rec (VarT cls))
        (VarT cls `appT` genRecordTy rec)
        [(mthd, gen)]
        []

-- | Generate a Proxy expression for the given type.
--
-- @genProxy [t|ty|]@ will result in a @Proxy :: Proxy ty@.
genProxy :: LHsType GhcPs -> LHsExpr GhcPs
genProxy ty = sigE (VarE Runtime.con_Proxy) (VarT Runtime.type_Proxy `appT` ty)
