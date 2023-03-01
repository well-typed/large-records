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
import Data.Record.Internal.Plugin.Names
import Data.Record.Internal.Plugin.Record

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

-- | Generate all large-records definitions for a record.
genLargeRecord :: MonadFresh m
  => QualifiedNames
  -> Record -> DynFlags -> m [LHsDecl GhcPs]
genLargeRecord names r@Record{..} dynFlags = concatM [
      (:[]) <$> genDatatype           r
    , genVectorConversions      names r
    , genIndexedAccessor        names r
    , genUnsafeSetIndex         names r
    , genStockInstances         names r
    , mapM (genHasFieldInstance names r) recordFields
    , sequence [
          genConstraintsClass    names r
        , genConstraintsInstance names r
        , genGenericInstance     names r dynFlags
        , genGHCGeneric          names r
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
      [ DerivClause (Just (withoutLoc (withDefExt AnyclassStrategy))) (c :| [])
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
genVectorConversions :: forall m.
     MonadFresh m
  => QualifiedNames -> Record -> m [LHsDecl GhcPs]
genVectorConversions QualifiedNames{..} r@Record{..} = concatM [
      fromVector
    , toVector
    ]
  where
    UnqualifiedNames{..} = getUnqualifiedNames

    fromVector :: m [LHsDecl GhcPs]
    fromVector = do
        args <- mapM (freshName . fieldName) recordFields
        return [
            sigD name $
              funT
                (recordTypeT r)
                (ConT type_AnyArray)
          , valD name $
              lamE1 (conP recordConName (map varP args)) $
                appE
                  (VarE anyArrayFromList)
                  (listE [ VarE noInlineUnsafeCo `appE` VarE arg
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
                (ConT type_AnyArray)
                (recordTypeT r)
          , valD name $
              lamE1 (varP x) $
                caseE
                  (VarE anyArrayToList `appE` VarE x)
                  [ ( listP (map varP args)
                    , appsE
                        (ConE recordConName)
                        [ VarE noInlineUnsafeCo `appE` VarE arg
                        | arg <- args
                        ]
                    )
                  , ( wildP
                    , VarE unq_error `appE` stringE matchErr
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
genIndexedAccessor ::
     MonadFresh m
  => QualifiedNames
  -> Record -> m [LHsDecl GhcPs]
genIndexedAccessor QualifiedNames{..} r@Record{..} = do
    x <- freshName' False $ mkTyVar  recordAnnLoc "x"
    n <- freshName $ mkExpVar recordAnnLoc "n"
    t <- freshName $ mkExpVar recordAnnLoc "t"
    return [
        sigD name $
          funT
            (ConT unq_type_Int)
            (recordTypeT r `funT` VarT x)
      , valD name $
          lamE (varP n :| [varP t]) $
            appE
              (VarE noInlineUnsafeCo)
              (appsE
                 (VarE anyArrayIndex)
                 [ VarE (nameVectorFrom r) `appE` VarE t
                 , VarE n
                 ]
              )
      ]
  where
    UnqualifiedNames{..} = getUnqualifiedNames

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
genUnsafeSetIndex ::
     MonadFresh m
  => QualifiedNames
  -> Record -> m [LHsDecl GhcPs]
genUnsafeSetIndex QualifiedNames{..} r@Record{..} = do
    x   <- freshName' False $ mkTyVar  recordAnnLoc "x"
    n   <- freshName $ mkExpVar recordAnnLoc "n"
    t   <- freshName $ mkExpVar recordAnnLoc "t"
    val <- freshName $ mkExpVar recordAnnLoc "val"
    return [
      sigD name $
               ConT unq_type_Int
        `funT` (recordTypeT r `funT` (VarT x `funT` recordTypeT r))
      , valD name $
          lamE (varP n :| [varP t, (varP val)]) $
            appE
              (VarE (nameVectorTo r))
              (appsE
                 (VarE anyArrayUpdate)
                 [ VarE (nameVectorFrom r) `appE` VarE t
                 , listE [
                       tupE $
                             VarE n
                         :| [VarE noInlineUnsafeCo `appE` VarE val]
                     ]
                 ]
              )
      ]
  where
    UnqualifiedNames{..} = getUnqualifiedNames

    name :: LRdrName
    name = nameUnsafeSetIndex r

-- | Generate 'HasField' instance for single field
--
-- Generates something like
--
-- > instance x ~ Word => HasField "tInt" (T a b) x where
-- >   hasField = \t -> (unsafeSetIndexT 0 t, unsafeGetIndexT 0 t)
genHasFieldInstance :: MonadFresh m
  => QualifiedNames
  -> Record -> Field -> m (LHsDecl GhcPs)
genHasFieldInstance QualifiedNames{..} r@Record{..} Field{..} = do
    x <- freshName' False $ mkTyVar  recordAnnLoc "x"
    t <- freshName $ mkExpVar recordAnnLoc "t"
    return $
      instanceD
        [equalP (VarT x) fieldType]
        (appsT
           (ConT type_HasField)
           [ stringT (nameBase fieldName)
           , recordTypeT r
           , VarT x
           ]
        )
        [ ( hasField
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
genConstraintsClass ::
     MonadFresh m
  => QualifiedNames -> Record -> m (LHsDecl GhcPs)
genConstraintsClass QualifiedNames{..} r@Record{..} = do
    c <- freshName' False $ mkTyVar recordAnnLoc "c"
    return $ classD
      []
      (nameConstraints r)
      (recordTyVars ++ [kindedTV c cKind])
      [ ( nameDictConstraints r
        , funT
            (ConT type_Proxy `appT` VarT c)
            (appsT
               (ConT type_Rep)
               [ ConT type_Dict `appT` VarT c
               , recordTypeT r
               ]
            )
        )
      ]
  where
    cKind :: LHsType GhcPs
    cKind = ConT type_Type `funT` ConT type_Constraint

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
genDict ::
     MonadFresh m
  => QualifiedNames
  -> Record -> m (LHsExpr GhcPs)
genDict names@QualifiedNames{..} Record{..} = do
    p <- freshName $ mkExpVar recordAnnLoc "p"
    return $
      lamE1 (varP p) $
        appE
          (VarE mkDicts)
          (listE (map (dictForField p) recordFields))
  where
    dictForField :: LRdrName -> Field -> LHsExpr GhcPs
    dictForField p Field{..} =
        appE
          (VarE noInlineUnsafeCo)
          (VarE mkDict `appsE` [VarE p, proxyE names fieldType])

-- | Generate (one and only) instance of the constraints class
--
-- Generates something like
--
-- > instance (..) => Constraints_T a b c where
-- >   dictConstraints_T = ..
--
-- where the body of @dictConstraints_T@ is generated by 'genDict'.
genConstraintsInstance ::
     MonadFresh m
  => QualifiedNames -> Record -> m (LHsDecl GhcPs)
genConstraintsInstance names r@Record{..} = do
    body <- genDict names r
    c    <- freshName' False $ mkTyVar recordAnnLoc "c"
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
genMetadata ::
     MonadFresh m
  => QualifiedNames
  -> Record -> DynFlags -> m (LHsExpr GhcPs)
genMetadata names@QualifiedNames{..} r@Record{..} dynFlags = do
    p <- freshName $ mkExpVar recordAnnLoc "p"
    return $
      lamE1 (varP p) $
        appsE (VarE mkMetadata) [
            stringE (nameRecord r)
          , stringE (nameBase recordConName)
          , listE (map auxField recordFields)
          ]
  where
    auxField :: Field -> LHsExpr GhcPs
    auxField Field{..} =
          (appE . VarE $ if isStrict dynFlags fieldStrictness
            then mkStrictField
            else mkLazyField)
        $ proxyE names
        $ stringT (nameBase fieldName)

isStrict :: DynFlags -> HsSrcBang -> Bool
isStrict dynFlags (HsSrcBang _ _ strictness) =
    case strictness of
      SrcStrict   -> True
      SrcLazy     -> False
      NoSrcStrict -> if strictData then True else False
  where
    strictData = xopt StrictData dynFlags

-- | Generate definition for `from` in the `Generic` instance
--
-- Generates something like
--
-- > repFromVectorStrict . vectorFromT
genFrom ::
     MonadFresh m
  => QualifiedNames
  -> Record -> m (LHsExpr GhcPs)
genFrom QualifiedNames{..} r@Record{..} = do
    x <- freshName $ mkExpVar recordAnnLoc "x"
    return $
      lamE1 (varP x) $
        VarE anyArrayToRep `appE` (VarE (nameVectorFrom r) `appE` VarE x)

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
genTo ::
     MonadFresh m
  => QualifiedNames
  -> Record -> m (LHsExpr GhcPs)
genTo QualifiedNames{..} r@Record{..} = do
    x <- freshName $ mkExpVar recordAnnLoc "x"
    return $
      lamE1 (varP x) $
        VarE (nameVectorTo r) `appE` (VarE anyArrayFromRep `appE` VarE x)

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
genGenericInstance ::
     MonadFresh m
  => QualifiedNames
  -> Record -> DynFlags -> m (LHsDecl GhcPs)
genGenericInstance names@QualifiedNames{..} r@Record{..} dynFlags  = do
    metadata <- genMetadata names r dynFlags
    from     <- genFrom     names r
    to       <- genTo       names r
    return $
      instanceD
        []
        (ConT type_LR_Generic `appT` recordTypeT r)
        [ ( lr_from     , from                         )
        , ( lr_to       , to                           )
        , ( lr_dict     , VarE (nameDictConstraints r) )
        , ( lr_metadata , metadata                     )
        ]
        [ tySynEqn type_LR_Constraints [recordTypeT r] $
            appsT
              (ConT (nameConstraints r))
              [VarT (tyVarBndrName v) | v <- recordTyVars]
        , tySynEqn type_LR_MetadataOf [recordTypeT r] $
            listT [
                tupT $ stringT (nameBase fieldName) :| [fieldType]
              | Field{..} <- recordFields
              ]
        ]

{-------------------------------------------------------------------------------
  "Stock" instances
-------------------------------------------------------------------------------}

-- | Generate stock instances
genStockInstances ::
     MonadFresh m
  => QualifiedNames
  -> Record -> m [LHsDecl GhcPs]
genStockInstances names r@Record{..} = concatM [
      genStockInstance names r d
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
genStockInstance :: MonadFresh m
  => QualifiedNames
  -> Record -> StockDeriving -> m [LHsDecl GhcPs]
genStockInstance QualifiedNames{..} r = pure . \case
    Show    -> [mkInstance unq_type_Show unq_showsPrec gshowsPrec]
    Eq      -> [mkInstance unq_type_Eq   unq_eq        geq       ]
    Ord     -> [mkInstance unq_type_Ord  unq_compare   gcompare  ]
    Generic -> []
  where
    UnqualifiedNames{..} = getUnqualifiedNames

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
genGHCGeneric ::
     MonadFresh m
  => QualifiedNames -> Record -> m (LHsDecl GhcPs)
genGHCGeneric QualifiedNames{..} r = pure $
    instanceD
      []
      (ConT type_GHC_Generic `appT` recordTypeT r)
      [ ( ghc_from , VarE wrapThroughLRGenerics   )
      , ( ghc_to   , VarE unwrapThroughLRGenerics )
      ]
      [ tySynEqn type_GHC_Rep [recordTypeT r] $
          ConT type_ThroughLRGenerics `appT` recordTypeT r
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
proxyE :: QualifiedNames -> LHsType GhcPs -> LHsExpr GhcPs
proxyE QualifiedNames{..} ty =
    sigE (VarE proxy) (ConT type_Proxy `appT` ty)

concatM :: Applicative m => [m [a]] -> m [a]
concatM = fmap concat . sequenceA
