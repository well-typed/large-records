{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The core of the plugin implementation.
module Data.Record.Internal.Plugin.CodeGen (genLargeRecord) where

import Data.List (nubBy)
import Data.List.NonEmpty (NonEmpty(..))

import Language.Haskell.TH (Extension(StrictData))

import qualified Data.Generics as SYB

import Data.Record.Internal.GHC.Fresh
import Data.Record.Internal.GHC.Shim hiding (mkTyVar)
import Data.Record.Internal.GHC.TemplateHaskellStyle
import Data.Record.Internal.Plugin.Record

import qualified Data.Record.Internal.Plugin.Names.GhcGenerics as GHC
import qualified Data.Record.Internal.Plugin.Names.Runtime     as RT

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

-- | Generate all large-records definitions for a record.
genLargeRecord :: MonadFresh m => Record -> DynFlags -> m [LHsDecl GhcPs]
genLargeRecord r@Record{..} dynFlags = concatM [
      (:[]) <$> genDatatype r
    , genVectorConversions  r
    , genIndexedAccessor    r
    , genUnsafeSetIndex     r
    , genStockInstances     r
    , mapM (genHasFieldInstance r) recordFields
    , sequence [
          genConstraintsClass    r
        , genConstraintsInstance r
        , genGenericInstance     r dynFlags
        , genGHCGeneric          r
      ]
    ]

{-------------------------------------------------------------------------------
  The type itself and conversion to and from vectors

  NOTE: All generation exampleshask assume as example

  > data T a b = MkT {
  >       tWord  :: Word
  >     , tBool  :: Bool
  >     , tChar  :: Char
  >     , tA     :: a
  >     , tListB :: [b]
  >     }
  >   deriving (Eq, Show)
-------------------------------------------------------------------------------}

-- | Generate the datatype that will represent the record
--
-- Currently this generates something like
--
-- > data T a b =
-- >      forall f0 f1 f2 f3 f4. (
-- >        f0 ~ Word
-- >      , f1 ~ Bool
-- >      , f2 ~ Char
-- >      , f3 ~ a
-- >      , f4 ~ [b]
-- >      )
-- >   => MkT {
-- >        tInt   :: f0
-- >      , tBool  :: f1
-- >      , tChar  :: f2
-- >      , tA     :: f3
-- >      , tListB :: f4
-- >      }
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
-- > newtype T a b = TFromVector {vectorFromT :: SmallArray Any}
-- >   deriving anyclass C -- where applicable
--
-- instead, along with a pattern synonym.
genDatatype :: MonadFresh m => Record -> m (LHsDecl GhcPs)
genDatatype Record{..} = pure $
    DataD
      recordTyName
      recordTyVars
      [ forallRecC
          vars
          (zipWith fieldContext vars recordFields)
          recordConName
          (zipWith fieldExistentialType vars recordFields)

      ]
      [ DerivClause (Just (noLoc (withDefExt AnyclassStrategy))) (c :| [])
      | DeriveAnyClass c <- recordDerivings
      ]
  where
    -- There is no need to generate fresh va  riables here, as these type vars
    -- cannot clash with anything else (no other type vars can be in scope).
    vars :: [LRdrName]
    vars = [
          mkTyVar recordAnnLoc ("lr_f" <> show i)
        | (i, _) <- zip [1 :: Int ..] recordFields
        ]

    optionalBang :: HsSrcBang -> LHsType GhcPs -> LHsType GhcPs
    optionalBang bang = noLocA . HsBangTy defExt bang

    fieldContext :: LRdrName -> Field -> LHsType GhcPs
    fieldContext var fld = equalP (VarT var) (fieldType fld)

    fieldExistentialType :: LRdrName -> Field -> (LRdrName, LHsType GhcPs)
    fieldExistentialType var fld = (fieldName fld, optionalBang (fieldStrictness fld) $ VarT var)

-- | Generate conversion to and from an array
--
-- Generates something like
--
-- > vectorFromT :: T a b -> SmallArray Any
-- > vectorFromT = \x ->
-- >     case x of
-- >       MkT f0 f1 f2 f3 f4 -> smallArrayFromList [
-- >           unsafeCoerce f0
-- >         , unsafeCoerce f1
-- >         , unsafeCoerce f2
-- >         , unsafeCoerce f3
-- >         , unsafeCoerce f4
-- >         ]
-- >
-- > vectorToT :: SmallArray Any -> T a b
-- > vectorToT = \x ->
-- >     case smallArrayToList x of
-- >       [f0, f1, f2, f3, f4] ->
-- >         MkT (unsafeCoerce f0)
-- >             (unsafeCoerce f1)
-- >             (unsafeCoerce f2)
-- >             (unsafeCoerce f3)
-- >             (unsafeCoerce f4)
-- >       _ -> error "Pattern match failure in vectorToT: vector with invalid number of elements."
--
-- TODO: From ghc 9.2, these could be identity functions. See 'genDatatype'
-- for details.
genVectorConversions :: forall m. MonadFresh m => Record -> m [LHsDecl GhcPs]
genVectorConversions r@Record{..} = concatM [
      fromVector
    , toVector
    ]
  where
    fromVector :: m [LHsDecl GhcPs]
    fromVector = do
        args <- mapM (freshName . fieldName) recordFields
        return [
            sigD name $
              funT
                (recordTypeT r)
                (ConT RT.type_SmallArray `appT` ConT RT.type_Any)
          , valD name $
              lamE1 (conP recordConName (map varP args)) $
                appE
                  (VarE RT.smallArrayFromList)
                  (listE [ VarE RT.unsafeCoerce `appE` VarE arg
                         | arg <- args
                         ]
                  )
          ]
     where
       name :: LRdrName
       name = nameVectorFrom r

    toVector :: m [LHsDecl GhcPs]
    toVector = do
        x    <- freshName $ mkExpVar recordAnnLoc "x"
        args <- mapM (freshName . fieldName) recordFields
        return $ [
            sigD name $
              funT
                (ConT RT.type_SmallArray `appT` ConT RT.type_Any)
                (recordTypeT r)
          , valD name $
              lamE1 (varP x) $
                caseE
                  (VarE RT.smallArrayToList `appE` VarE x)
                  [ ( listP (map varP args)
                    , appsE
                        (ConE recordConName)
                        [ VarE RT.unsafeCoerce `appE` VarE arg
                        | arg <- args
                        ]
                    )
                  , ( wildP
                    , VarE RT.error `appE` stringE matchErr
                    )
                  ]
          ]
      where
        name :: LRdrName
        name = nameVectorTo r

        matchErr :: String
        matchErr = concat [
              "Pattern match failure in "
            , nameBase (nameVectorTo r)
            , ": vector with invalid number of elements."
            ]

{-------------------------------------------------------------------------------
  Field accessors and 'HasField' instance

  TODO: If we had support within GHC itself for accessing fields in records,
  we might be able to integrate this a lot more closely with normal GHC,
  especially when combined with the @NoFieldSelectors@ extension.

  See <https://gitlab.haskell.org/ghc/ghc/-/issues/17991>
-------------------------------------------------------------------------------}

-- | Generate the indexed field accessor
--
-- Generates something like
--
-- > unsafeGetIndexT :: forall x a b. Int -> T a b -> x
-- > unsafeGetIndexT = \ n t -> noInlineUnsafeCo (V.unsafeIndex (vectorFromT t) n)
genIndexedAccessor :: MonadFresh m => Record -> m [LHsDecl GhcPs]
genIndexedAccessor r@Record{..} = do
    x <- freshName $ mkTyVar  recordAnnLoc "x"
    n <- freshName $ mkExpVar recordAnnLoc "n"
    t <- freshName $ mkExpVar recordAnnLoc "t"
    return [
        sigD name $
          funT
            (ConT RT.type_Int)
            (recordTypeT r `funT` VarT x)
      , valD name $
          lamE (varP n :| [varP t]) $
            appE
              (VarE RT.noInlineUnsafeCo)
              (appsE
                 (VarE RT.indexSmallArray)
                 [ VarE (nameVectorFrom r) `appE` VarE t
                 , VarE n
                 ]
              )
      ]
  where
    name :: LRdrName
    name = nameUnsafeGetIndex r

-- | Generate index field overwrite
--
-- Generates something like
--
-- > unsafeSetIndexT :: forall x a b. Int -> T a b -> x -> T a b
-- > unsafeSetIndexT = \n t val ->
-- >     vectorToT (V.unsafeUpd (vectorFromT t) [(n, noInlineUnsafeCo val)])
--
-- NOTE: Like 'genTo', this function used to be more complicated, because it
-- would need to take the strictness of the fields into account. If we change
-- our internal representation, we might need to be more careful with that
-- again. See 'genTo' for further discussion.
genUnsafeSetIndex :: MonadFresh m => Record -> m [LHsDecl GhcPs]
genUnsafeSetIndex r@Record{..} = do
    x   <- freshName $ mkTyVar  recordAnnLoc "x"
    n   <- freshName $ mkExpVar recordAnnLoc "n"
    t   <- freshName $ mkExpVar recordAnnLoc "t"
    val <- freshName $ mkExpVar recordAnnLoc "val"
    return [
      sigD name $
               ConT RT.type_Int
        `funT` (recordTypeT r `funT` (VarT x `funT` recordTypeT r))
      , valD name $
          lamE (varP n :| [varP t, (varP val)]) $
            appE
              (VarE (nameVectorTo r))
              (appsE
                 (VarE RT.updateSmallArray)
                 [ VarE (nameVectorFrom r) `appE` VarE t
                 , listE [
                       tupE $
                             VarE n
                         :| [VarE RT.noInlineUnsafeCo `appE` VarE val]
                     ]
                 ]
              )
      ]
  where
    name :: LRdrName
    name = nameUnsafeSetIndex r

-- | Generate 'HasField' instance for single field
--
-- Generates something like
--
-- > instance x ~ Word => HasField "tInt" (T a b) x where
-- >   hasField = \t -> (unsafeSetIndexT 0 t, unsafeGetIndexT 0 t)
genHasFieldInstance :: MonadFresh m => Record -> Field -> m (LHsDecl GhcPs)
genHasFieldInstance r@Record{..} Field{..} = do
    x <- freshName $ mkTyVar  recordAnnLoc "x"
    t <- freshName $ mkExpVar recordAnnLoc "t"
    return $
      instanceD
        [equalP (VarT x) fieldType]
        (appsT
           (ConT RT.type_HasField)
           [ stringT (nameBase fieldName)
           , recordTypeT r
           , VarT x
           ]
        )
        [ ( RT.unq_hasField
          , lamE1 (varP t) $
              tupE $
                    appsE (VarE (nameUnsafeSetIndex r)) [intE fieldIndex, VarE t]
                :| [appsE (VarE (nameUnsafeGetIndex r)) [intE fieldIndex, VarE t]]
          )
        ]
        []

{-------------------------------------------------------------------------------
  Generics
-------------------------------------------------------------------------------}

-- | Generate the class we will use to instantiate 'Constraints'
--
-- Generates something like this:
--
-- > class Constraints_T a b (c :: Type -> Constraint) where
-- >   dictConstraints_T :: Proxy c -> Rep (Dict c) (T a b)
--
-- NOTE: It is critical that we don't give the class any superclass constraints
-- like
--
-- > class (c Word, c Bool, c Char, c a, c [b])
-- >    => Constraints_T a b (c :: Type -> Constraint)
--
-- because then @ghc@ would use resolve @Constraints_T@ to that tuple instead,
-- and use lots of "tuple constraint extractor" functions, each of which have
-- the same size as the number of constraints (another example of a
-- @case f of { T x1 x2 x3 .. -> xn@ function, but now at the dictionary level).
genConstraintsClass :: MonadFresh m => Record -> m (LHsDecl GhcPs)
genConstraintsClass r@Record{..} = do
    c <- freshName $ mkTyVar recordAnnLoc "c"
    return $ classD
      []
      (nameConstraints r)
      (recordTyVars ++ [kindedTV c cKind])
      [ ( nameDictConstraints r
        , funT
            (ConT RT.type_Proxy `appT` VarT c)
            (appsT
               (ConT RT.type_Rep)
               [ ConT RT.type_Dict `appT` VarT c
               , recordTypeT r
               ]
            )
        )
      ]
  where
    cKind :: LHsType GhcPs
    cKind = ConT RT.type_Type `funT` ConT RT.type_Constraint

-- | Superclass constraints required by the constraints class instance
--
-- Generates something like
--
-- > (c Word, c Bool, c Char, c a, c [b])
--
-- However, we filter out constraints that are type variable free, so if we
-- pass, say, @Show@ for @c@, then we generate
--
-- > (Show a, Show [b])
--
-- instead. This avoids @ghc@ complaining about
--
-- > Redundant constraints: (Show Word, Show Bool, Show Char)
genRequiredConstraints :: Record -> LHsType GhcPs -> [LHsType GhcPs]
genRequiredConstraints Record{..} c =
    nubBy sameType $ filter hasTypeVar $ map constrainField recordFields
  where
    constrainField :: Field -> LHsType GhcPs
    constrainField Field{..} = c `appT` fieldType

    sameType :: LHsType GhcPs -> LHsType GhcPs -> Bool
    sameType = compareHs

    hasTypeVar :: LHsType GhcPs -> Bool
    hasTypeVar = not . null . allTyVars

    allTyVars :: LHsType GhcPs -> [String]
    allTyVars = SYB.everything (++) (SYB.mkQ [] isTypeVar)

    isTypeVar :: LHsType GhcPs -> [String]
    isTypeVar (VarT (TyVar name)) = [name]
    isTypeVar _otherwise          = []

-- | Generate the dictionary creation function ('dict')
--
-- Generates something like
--
-- > \p -> Rep (V.fromList [
-- >     noInlineUnsafeCo (dictFor p (Proxy :: Proxy Word))
-- >   , noInlineUnsafeCo (dictFor p (Proxy :: Proxy Bool))
-- >   , noInlineUnsafeCo (dictFor p (Proxy :: Proxy Char))
-- >   , noInlineUnsafeCo (dictFor p (Proxy :: Proxy a))
-- >   , noInlineUnsafeCo (dictFor p (Proxy :: Proxy [b]))
-- >   ])
genDict :: MonadFresh m => Record -> m (LHsExpr GhcPs)
genDict Record{..} = do
    p <- freshName $ mkExpVar recordAnnLoc "p"
    return $
      lamE1 (varP p) $
        appE
          (ConE RT.con_Rep)
          (appE
             (VarE RT.smallArrayFromList)
             (listE (map (dictForField p) recordFields))
          )
  where
    dictForField :: LRdrName -> Field -> LHsExpr GhcPs
    dictForField p Field{..} =
        appE
          (VarE RT.noInlineUnsafeCo)
          (VarE RT.dictFor `appsE` [VarE p, proxyE fieldType])

-- | Generate (one and only) instance of the constraints class
--
-- Generates something like
--
-- > instance (..) => Constraints_T a b c where
-- >   dictConstraints_T = ..
--
-- where the body of @dictConstraints_T@ is generated by 'genDict'.
genConstraintsInstance :: MonadFresh m => Record -> m (LHsDecl GhcPs)
genConstraintsInstance r@Record{..} = do
    body <- genDict r
    c    <- freshName $ mkTyVar recordAnnLoc "c"
    return $
      instanceD
        (genRequiredConstraints r (VarT c))
        (appsT
           (ConT (nameConstraints r))
           ([VarT (tyVarBndrName v) | v <- recordTyVars] ++ [VarT c]))
        [(nameDictConstraints r, body)]
        []

-- | Generate metadata
--
-- Generates something like
--
-- > \_p  -> Metadata {
-- >     recordName          = "T"
-- >   , recordConstructor   = "MkT"
-- >   , recordSize          = 5
-- >   , recordFieldMetadata = Rep $ V.fromList [
-- >         FieldMetadata (Proxy :: Proxy "tInt"))   FieldLazy
-- >       , FieldMetadata (Proxy :: Proxy "tBool"))  FieldLazy
-- >       , FieldMetadata (Proxy :: Proxy "tChar"))  FieldLazy
-- >       , FieldMetadata (Proxy :: Proxy "tA"))     FieldLazy
-- >       , FieldMetadata (Proxy :: Proxy "tListB")) FieldLazy
-- >       ]
-- >   }
genMetadata :: MonadFresh m => Record -> DynFlags -> m (LHsExpr GhcPs)
genMetadata r@Record{..} dynFlags = do
    p <- freshName $ mkExpVar recordAnnLoc "p"
    return $
      lamE1 (varP p) $
        recConE
          RT.con_Metadata [
              ( RT.recordName
              , stringE (nameRecord r)
              )
            , ( RT.recordConstructor
              , stringE (nameBase recordConName)
              )
            , ( RT.recordSize
              , intE (length recordFields)
              )
            , ( RT.recordFieldMetadata
              , appE
                  (ConE RT.con_Rep)
                  (appE
                     (VarE RT.smallArrayFromList)
                     (listE (map auxField recordFields))
                  )
              )
            ]
  where
    auxField :: Field -> LHsExpr GhcPs
    auxField Field{..} =
        appsE
          (ConE RT.con_FieldMetadata)
          [ proxyE (stringT (nameBase fieldName))
          , ConE $ case decideStrictness dynFlags fieldStrictness of
              HsStrict                  -> RT.con_FieldStrict
              HsLazy                    -> RT.con_FieldLazy
              HsUnpack _                -> RT.con_FieldStrict
          ]

-- | Implementation of https://hackage.haskell.org/package/base-4.16.1.0/docs/GHC-Generics.html#t:DecidedStrictness
decideStrictness :: DynFlags -> HsSrcBang -> HsImplBang
decideStrictness dynFlags (HsSrcBang _ unpackedness strictness) =
  case (unpackedness, srcToImpl strictness) of
    (SrcUnpack, HsStrict) | optimizations -> HsUnpack Nothing
    (_, strictness') -> strictness'
  where
    strictData = xopt StrictData dynFlags
    optimizations = optLevel dynFlags >= 1
    srcToImpl = \case
      SrcStrict -> HsStrict
      SrcLazy -> HsLazy
      NoSrcStrict -> if strictData then HsStrict else HsLazy

-- | Generate definition for `from` in the `Generic` instance
--
-- Generates something like
--
-- > repFromVectorStrict . vectorFromT
genFrom :: MonadFresh m => Record -> m (LHsExpr GhcPs)
genFrom r@Record{..} = do
    x <- freshName $ mkExpVar recordAnnLoc "x"
    return $
      lamE1 (varP x) $
        VarE RT.repFromVector `appE` (VarE (nameVectorFrom r) `appE` VarE x)

-- | Generate definition for `to` in the `Generic` instance
--
-- > vectorToT . repToVector
--
-- NOTE: This function used to be more complicated. When the internal
-- representation of a record /is/ a vector, then we have to be very careful
-- with the strictness of the fields here. However, since we currently use a
-- " normal " record as our internal representation (albeit with strange types),
-- and the fields of that record have their own strictness annotation, we don't
-- have to worry about strictness here.
genTo :: MonadFresh m => Record -> m (LHsExpr GhcPs)
genTo r@Record{..} = do
    x <- freshName $ mkExpVar recordAnnLoc "x"
    return $
      lamE1 (varP x) $
        VarE (nameVectorTo r) `appE` (VarE RT.repToVector `appE` VarE x)

-- | Generate an instance of large-records 'Data.Record.Generic'.
--
-- In the sample instance below, @vectorFromT@ and @vectorToT@ are generated
-- per-record by 'genVectorFrom' and 'genVectorTo' respectively.
--
-- > instance Generic (T ...) where
-- >   type Constraints (T ...) = Constraints_T ...
-- >   type MetadataOf  (T ...) = '[ '("field1", fieldType1), ... ]
-- >
-- >   from     = ..
-- >   to       = ..
-- >   dict     = dictConstraints_T
-- >   metadata = ..
genGenericInstance :: MonadFresh m => Record -> DynFlags -> m (LHsDecl GhcPs)
genGenericInstance r@Record{..} dynFlags  = do
    metadata <- genMetadata r dynFlags
    from     <- genFrom     r
    to       <- genTo       r
    return $
      instanceD
        []
        (ConT RT.type_Generic `appT` recordTypeT r)
        [ ( RT.unq_from     , from                         )
        , ( RT.unq_to       , to                           )
        , ( RT.unq_dict     , VarE (nameDictConstraints r) )
        , ( RT.unq_metadata , metadata                     )
        ]
        [ tySynEqn RT.unq_type_Constraints [recordTypeT r] $
            appsT
              (ConT (nameConstraints r))
              [VarT (tyVarBndrName v) | v <- recordTyVars]
        , tySynEqn RT.unq_type_MetadataOf [recordTypeT r] $
            listT [
                tupT $ stringT (nameBase fieldName) :| [fieldType]
              | Field{..} <- recordFields
              ]
        ]
  where

{-------------------------------------------------------------------------------
  "Stock" instances
-------------------------------------------------------------------------------}

-- | Generate stock instances
genStockInstances :: MonadFresh m => Record -> m [LHsDecl GhcPs]
genStockInstances r@Record{..} = concatM [
      genStockInstance r d
    | DeriveStock d <- recordDerivings
    ]

-- | For a record type @T@ and stock-derivable class @C@, generate
--
-- > instance $(genRequiredConstraints T C) => C T where
-- >   $(method) = $(generic implementation)
--
-- NOTE: All of these instances depend on the 'Data.Record.Generics.Generics'
-- instance.
--
-- TODO: For 'Generic' we currently don't do anything. We could change this so
-- that we generate the 'GHC.Generics' instance only when the user asks for a
-- 'Generics' instance?
genStockInstance :: MonadFresh m => Record -> StockDeriving -> m [LHsDecl GhcPs]
genStockInstance r = pure . \case
    Show    -> [mkInstance RT.type_Show RT.unq_showsPrec RT.gshowsPrec]
    Eq      -> [mkInstance RT.type_Eq   RT.unq_eq        RT.geq       ]
    Ord     -> [mkInstance RT.type_Ord  RT.unq_compare   RT.gcompare  ]
    Generic -> []
  where
    mkInstance :: LRdrName -> LRdrName -> LRdrName -> LHsDecl GhcPs
    mkInstance cls mthd gen =
        instanceD
          (genRequiredConstraints r (ConT cls))
          (ConT cls `appT` recordTypeT r)
          [(mthd, VarE gen)]
          []

{-------------------------------------------------------------------------------
  GHC generics
-------------------------------------------------------------------------------}

-- | Generate GHC generics instance
--
-- Generates something like
--
-- > instance GHC.Generic ExampleRecord where
-- >   type Rep ExampleRecord = ThroughLRGenerics ExampleRecord
-- >
-- >   from = WrapThroughLRGenerics
-- >   to   = unwrapThroughLRGenerics
--
-- See 'ThroughLRGenerics' for documentation.
genGHCGeneric :: MonadFresh m => Record -> m (LHsDecl GhcPs)
genGHCGeneric r = pure $
    instanceD
      []
      (ConT GHC.type_Generic `appT` recordTypeT r)
      [ ( GHC.unq_from , ConE RT.con_WrapThroughLRGenerics )
      , ( GHC.unq_to   , VarE RT.unwrapThroughLRGenerics   )
      ]
      [ tySynEqn GHC.unq_type_Rep [recordTypeT r] $
          ConT RT.type_ThroughLRGenerics `appT` recordTypeT r
      ]

{-------------------------------------------------------------------------------
  Auxiliary functions for dealing with records
-------------------------------------------------------------------------------}

-- | The saturated type of the record (that is, with all type vars applied)
recordTypeT :: Record -> LHsType GhcPs
recordTypeT Record{..} =
    ConT recordTyName `appsT` [VarT (tyVarBndrName f) | f <- recordTyVars]

{-------------------------------------------------------------------------------
  Pick names for generated code
-------------------------------------------------------------------------------}

nameRecord :: Record -> String
nameRecord Record{..} = nameBase recordTyName

-- | Make name derived from the name of the record
mkDerived :: (SrcSpan -> String -> LRdrName) -> String -> Record -> LRdrName
mkDerived f prefix r = f (recordAnnLoc r) (prefix <> nameRecord r)

nameVectorFrom      :: Record -> LRdrName
nameVectorTo        :: Record -> LRdrName
nameUnsafeGetIndex  :: Record -> LRdrName
nameUnsafeSetIndex  :: Record -> LRdrName
nameConstraints     :: Record -> LRdrName
nameDictConstraints :: Record -> LRdrName

nameVectorFrom      = mkDerived mkExpVar "vectorFrom"
nameVectorTo        = mkDerived mkExpVar "vectorTo"
nameUnsafeGetIndex  = mkDerived mkExpVar "unsafeGetIndex"
nameUnsafeSetIndex  = mkDerived mkExpVar "unsafeSetIndex"
nameConstraints     = mkDerived mkTyCon  "Constraints_"
nameDictConstraints = mkDerived mkExpVar "dictConstraints_"

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | Generate a Proxy expression for the given type.
--
-- @proxyE [t|ty|]@ will result in a @Proxy :: Proxy ty@.
proxyE :: LHsType GhcPs -> LHsExpr GhcPs
proxyE ty = sigE (ConE RT.con_Proxy) (ConT RT.type_Proxy `appT` ty)

concatM :: Applicative m => [m [a]] -> m [a]
concatM = fmap concat . sequenceA
