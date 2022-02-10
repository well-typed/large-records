{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ParallelListComp #-}

-- | The core of the plugin implementation.
module Data.Record.Plugin.CodeGen (genLargeRecord) where

import Control.Monad (when)
import qualified Data.Generics.Uniplate.Data as Uniplate
import Data.Record.Plugin.GHC
import Data.Record.Plugin.RuntimeNames as Runtime
import Data.Record.Plugin.Types.Options (shouldGeneratedHasField, shouldRecordBeStrict)
import Data.Record.Plugin.Types.Record (Record (..), RecordDeriving (..), StockDeriving (..))

recordName :: Record -> String
recordName Record {tyName} = occNameString (rdrNameOcc tyName)

nameVectorFrom :: Record -> RdrName
nameVectorFrom rec = varRdr ("vectorFrom" <> recordName rec)

nameVectorTo :: Record -> RdrName
nameVectorTo rec = varRdr ("vectorTo" <> recordName rec)

nameUnsafeGetIndex :: Record -> RdrName
nameUnsafeGetIndex rec = varRdr ("unsafeGetIndex" <> recordName rec)

nameUnsafeSetIndex :: Record -> RdrName
nameUnsafeSetIndex rec = varRdr ("unsafeSetIndex" <> recordName rec)

nameConstraints :: Record -> RdrName
nameConstraints rec = mkRdrUnqual (mkTcOcc ("Constraints_" <> recordName rec))

nameDictConstraints :: Record -> RdrName
nameDictConstraints rec = varRdr ("dictConstraints_" <> recordName rec)

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
genRecordTy Record {tyName, tyVars} = varT tyName `appsT` [varT (hsTyVarName f) | f <- tyVars]

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
genDatatype :: Record -> LHsDecl GhcPs
genDatatype Record {tyName, conName, tyVars, fields, derivings, options} =
  noLoc
    ( TyClD
        noExtField
        DataDecl
          { tcdDExt = noExtField,
            tcdLName = noLoc tyName,
            tcdTyVars = mkHsQTvs (noLoc <$> tyVars),
            tcdFixity = Prefix,
            tcdDataDefn =
              HsDataDefn
                { dd_ext = noExtField,
                  dd_ND = DataType,
                  dd_ctxt = noLoc [],
                  dd_cType = Nothing,
                  dd_kindSig = Nothing,
                  dd_cons =
                    [ noLoc
                        ConDeclH98
                          { con_ext = noExtField,
                            con_name = noLoc conName,
                            con_forall = noLoc True,
                            con_ex_tvs = [noLoc (UserTyVar noExtField (noLoc v)) | v <- vars],
                            con_mb_cxt = Just (noLoc [mkEqConstr v t | v <- vars | (_, t) <- fields]),
                            con_args = RecCon (noLoc [mkRecField f v | v <- vars | (f, _) <- fields]),
                            con_doc = Nothing
                          }
                    ],
                  dd_derivs = noLoc [anyclassDeriving c | DeriveAnyClass c <- derivings]
                }
          }
    )
  where
    vars = [varRdrT ("lr_f" <> show i) | (i, _) <- zip [1 ..] fields]

    mkEqConstr var ty = opT (varT var) typeEq (noLoc ty)
    mkRecField field var = conDeclField field (optionalBang (varT var))
    optionalBang = if shouldRecordBeStrict options then bangT else id

-- | Generate @vectorFromT@ for record @T@.
--
-- > vectorFromT :: T ... -> Vector Any
-- > vectorFromT = \(T field1 ...) -> V.fromList [unsafeCoerce field1, ...]
--
-- TODO: From GHC 9.2, this could be an identify function after changing the record representation.
genVectorFrom :: Record -> [LHsDecl GhcPs]
genVectorFrom rec@Record {tyName, conName, tyVars, fields} =
  let body =
        lamE
          [conP conName [varP f | (f, _) <- fields]]
          (varE Runtime.fromList `appE` listE [varE Runtime.unsafeCoerce `appE` varE f | (f, _) <- fields])
   in simpleFn (nameVectorFrom rec) (genRecordTy rec `arrT` (varT Runtime.type_Vector `appT` varT Runtime.type_Any)) body

-- | Generate @vectorToT@ for record @T@.
--
-- > vectorToT :: Vector Any -> T ...
-- > vectorToT = \x -> case toList x of
-- >     [field1, ...] -> T (unsafeCoerce field1) ...
-- >     _ -> error "Pattern match failure in vectorToT: vector with invalid number of elements."
--
-- TODO: From GHC 9.2, this could be an identify function after changing the record representation.
genVectorTo :: Record -> [LHsDecl GhcPs]
genVectorTo rec@Record {tyName, conName, tyVars, fields} =
  let body =
        lamE [varP nameArg] do
          caseE
            (varE Runtime.toList `appE` varE nameArg)
            [ (listP [varP f | (f, _) <- fields], appsE (varE conName) [varE Runtime.unsafeCoerce `appE` varE f | (f, _) <- fields]),
              (wildP, varE Runtime.error `appE` stringE matchErr)
            ]
   in simpleFn (nameVectorTo rec) ((varT Runtime.type_Vector `appT` varT Runtime.type_Any) `arrT` genRecordTy rec) body
  where
    nameArg = varRdr "x"
    matchErr =
      concat
        [ "Pattern match failure in ",
          rdrNameString (nameVectorTo rec),
          ": vector with invalid number of elements."
        ]

-- | Generate @unsafeGetIndexT@ for record @T@.
--
-- > unsafeGetIndexT :: Int -> T ... -> val
-- > unsafeGetIndexT = \index arg -> noInlineUnsafeCo (V.unsafeIndex (Main.vectorFromB arg) index)
genUnsafeGetIndex :: Record -> [LHsDecl GhcPs]
genUnsafeGetIndex rec =
  simpleFn
    (nameUnsafeGetIndex rec)
    (varT Runtime.type_Int `arrT` (genRecordTy rec `arrT` varT (varRdrT "lr_result_")))
    ( lamE
        [varP index, varP arg]
        (varE Runtime.noInlineUnsafeCo `appE` varE Runtime.unsafeIndex `appsE` [varE (nameVectorFrom rec) `appE` varE arg, varE index])
    )
  where
    index = varRdr "index"
    arg = varRdr "arg"

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
genUnsafeSetIndex rec =
  simpleFn
    (nameUnsafeSetIndex rec)
    (varT Runtime.type_Int `arrT` (genRecordTy rec `arrT` (varT (varRdrT "lr_result_") `arrT` genRecordTy rec)))
    ( lamE
        [varP index, varP arg, bangP (varP val)]
        ( varE (nameVectorTo rec)
            `appE` ( varE Runtime.unsafeUpd
                       `appsE` [ varE (nameVectorFrom rec) `appE` varE arg,
                                 listE [tupleE [varE index, varE Runtime.noInlineUnsafeCo `appE` varE val]]
                               ]
                   )
        )
    )
  where
    index = varRdr "index"
    arg = varRdr "arg"
    val = varRdr "val"

-- | Generate a @HasField@ instance for the @i@-th field of the record with given name and type.
--
-- @unsafeGetIndexT@ and @unsafeSetIndexT@ are generated per-record by 'genUnsafeGetIndex' and 'genUnsafeSetIndex' respectively.
--
-- > instance t ~ fieldType => HasField "fieldName" (T ...) t where
-- >   hasField = \arg -> (unsafeSetIndexT i arg, unsafeGetIndexT i arg)
genHasFieldInstance :: Record -> Int -> (RdrName, HsType GhcPs) -> LHsDecl GhcPs
genHasFieldInstance rec index (fieldName, fieldTy) =
  instanceD_simple
    [opT (varT fieldTyVar) typeEq (noLoc fieldTy)]
    (varT Runtime.type_HasField `appsT` [stringT fieldStr, genRecordTy rec, varT fieldTyVar])
    [ simpleBind
        (varRdr "hasField")
        ( lamE
            [varP arg]
            ( tupleE
                [ appsE (varE (nameUnsafeSetIndex rec)) [intE index, varE arg],
                  appsE (varE (nameUnsafeGetIndex rec)) [intE index, varE arg]
                ]
            )
        )
    ]
    []
  where
    fieldStr = rdrNameString fieldName
    arg = varRdr "arg"
    fieldTyVar = varRdrT "lr_field_ty"

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
  (classD_simple [] (nameConstraints rec) (map noLoc tyVars ++ [kindedTyVarBndr c (varT Runtime.type_Type `arrT` varT Runtime.type_Constraint)]))
    [ classOpSig (nameDictConstraints rec) ((varT Runtime.type_Proxy `appT` varT c) `arrT` (varT Runtime.type_Rep `appsT` [varT Runtime.type_Dict `appT` varT c, genRecordTy rec]))
    ]
  where
    c = mkRdrUnqual (mkTyVarOcc "lr_con_c")

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
  instanceD_simple
    [varT c `appT` noLoc ty | (_, ty) <- fields]
    (appsT (varT (nameConstraints rec)) ([varT (hsTyVarName v) | v <- tyVars] ++ [varT c]))
    [simpleBind (nameDictConstraints rec) body]
    []
  where
    c = mkRdrUnqual (mkTyVarOcc "lr_con_c")
    p = mkRdrUnqual (mkVarOcc "p")
    body = lamE [varP p] (varE Runtime.con_Rep `appE` (varE Runtime.fromList `appE` listE dicts))
    dicts = [mkDict (noLoc ty) | (_, ty) <- fields]

    mkDict :: LHsType GhcPs -> LHsExpr GhcPs
    mkDict ty = varE Runtime.noInlineUnsafeCo `appE` (varE Runtime.dictFor `appsE` [varE p, genProxy ty])

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
  instanceD_simple
    []
    (varT Runtime.type_Generic `appT` genRecordTy rec)
    [ simpleBind
        Runtime.from_unqual
        (lamE [varP x] (varE Runtime.repFromVector `appE` (varE (nameVectorFrom rec) `appE` varE x))),
      simpleBind
        Runtime.to_unqual
        ( lamE
            [varP x]
            ( appE
                (lamE [varP y] (varE Runtime.seq `appsE` [varE Runtime.rnfVectorAny `appE` varE y, varE (nameVectorTo rec) `appE` varE y]))
                (varE Runtime.repToVector `appE` varE x)
            )
        ),
      simpleBind Runtime.dict_unqual (varE (nameDictConstraints rec)),
      simpleBind Runtime.metadata_unqual do
        lamE [varP x] do
          (recConE Runtime.con_Metadata)
            [ Runtime.field_recordName `recFieldE` stringE (recordName rec),
              Runtime.field_recordConstructor `recFieldE` stringE (rdrNameString conName),
              Runtime.field_recordSize `recFieldE` intE (length fields),
              Runtime.field_recordFieldMetadata `recFieldE` (varE Runtime.con_Rep `appE` (varE Runtime.fromList `appE` metadata))
            ]
    ]
    [ tfInstanceD Runtime.type_Constraints_unqual [genRecordTy rec] constraints,
      tfInstanceD Runtime.type_MetadataOf_unqual [genRecordTy rec] metadataOf
    ]
  where
    (x, y) = (varRdr "x", varRdr "y")

    constraints = appsT (varT (nameConstraints rec)) [varT (hsTyVarName v) | v <- tyVars]
    metadataOf = listT [tupleT [stringT (rdrNameString f), noLoc t] | (f, t) <- fields]
    metadata = listE [mkFieldMD name | (name, _) <- fields]
    mkFieldMD name =
      (appsE (varE Runtime.con_FieldMetadata))
        [ genProxy (stringT (rdrNameString name)),
          varE Runtime.con_FieldStrict
        ]

-- | Generate an instance of @GHC.Generics.Generic@ that goes through large-records generics.
--
-- > instance Generic (T ...) where
-- >   type Rep (T ...) = ThroughLRGenerics (T ...)
-- >   from = WrapThroughLRGenerics
-- >   to = unwrapThroughLRGenerics
genGHCGeneric :: Record -> LHsDecl GhcPs
genGHCGeneric rec =
  (instanceD_simple [] (varT Runtime.type_GHC_Generic `appT` genRecordTy rec))
    [ simpleBind Runtime._GHC_from_unqual (varE Runtime.con_WrapThroughLRGenerics),
      simpleBind Runtime._GHC_to_unqual (varE Runtime.unwrapThroughLRGenerics)
    ]
    [ tfInstanceD Runtime.type_GHC_Rep_unqual [genRecordTy rec] (varT Runtime.type_ThroughLRGenerics `appT` genRecordTy rec)
    ]

-- | @genRequiredContext r c@ generates a list of constraints @[c t1, c t2, ...]@ for all type variables in the definition of @r@.
--
-- TODO: 'nub' the type variables.
--
-- TODO: @genRequiredConstraints@ in the Template Haskell version is smarter and generates e.g. @c [b]@ instead of @c b@. This might matter for
-- phantom type variables.
genRequiredContext :: Record -> LHsType GhcPs -> [LHsType GhcPs]
genRequiredContext Record {fields} c =
  [c `appT` noLoc t | (_, t) <- fields, hasTypeVars t]
  where
    hasTypeVars :: HsType GhcPs -> Bool
    hasTypeVars = any (isTvOcc . rdrNameOcc) . Uniplate.universeBi

-- | Generate stock instances for the classes listes in the record's 'derivings' field.
genStockInstances :: Record -> [LHsDecl GhcPs]
genStockInstances rec@Record {derivings} = do
  DeriveStock d <- derivings
  case genStockInstance rec d of
    Just decl -> pure decl
    Nothing -> []

-- | For a record type @T@ and stock-derivable class @C@, generate
--
-- > instance $(genRequiredContext T C) => C T where
-- >   $(method) = $(generic implementation)
--
-- Note: doesn't do anything for 'Generic' because we want to generate a custom instance instead. See 'genGHCGeneric'.
genStockInstance :: Record -> StockDeriving -> Maybe (LHsDecl GhcPs)
genStockInstance rec = \case
  Show -> Just (mkInstance Runtime.type_Show Runtime.showsPrec (varE Runtime.gshowsPrec))
  Eq -> Just (mkInstance Runtime.type_Eq Runtime.eq (varE Runtime.geq))
  Ord -> Just (mkInstance Runtime.type_Ord Runtime.compare (varE Runtime.gcompare))
  Generic -> Nothing
  where
    mkInstance cls mthd gen =
      instanceD_simple
        (genRequiredContext rec (varT cls))
        (varT cls `appT` genRecordTy rec)
        [simpleBind mthd gen]
        []

-- | Generate a Proxy expression for the given type.
--
-- @genProxy [t|ty|]@ will result in a @Proxy :: Proxy ty@.
genProxy :: LHsType GhcPs -> LHsExpr GhcPs
genProxy ty = typeSigE (varE Runtime.con_Proxy) (varT Runtime.type_Proxy `appT` ty)
