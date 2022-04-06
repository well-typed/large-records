{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fplugin=RecordDotPreprocessor -fplugin=Data.Record.Anon.Plugin #-}

module Test.Sanity.BlogPost (
    tests
    -- * Examples for the blog post
  , checkIsSubRow
  , exampleRender
  , exampleRender'
  , magenta
  , magenta'
  , ordImpliesEq
  , purple
  , recordToJSON
  , recordToJSON'
  , reduceBlue
  , reduceGreen
  , reduceRed
  , showRecord
  , smallerSatisfies
  ) where

import Data.Aeson (ToJSON(..), Value)
import Data.Bifunctor
import Data.Function (on)
import Data.Kind
import Data.Maybe
import Data.SOP (fn_3)
import Data.String
import Data.Type.Equality
import GHC.TypeLits
import Optics.Core (over)
import Text.Parsec (Parsec, ParseError)
import Text.Read (readMaybe)

import qualified Data.Aeson                 as Aeson
import qualified Data.Aeson.Encode.Pretty   as Aeson.Pretty
import qualified Data.ByteString.Lazy       as Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as Lazy.BS.C8
import qualified Data.Text                  as Text
import qualified Text.Parsec                as Parsec

import Data.Record.Anon
import Data.Record.Anon.Simple (Record)
import Data.Record.Anon.Advanced (InRow(..))

import qualified Data.Record.Anon.Advanced  as A
import qualified Data.Record.Anon.Simple    as S

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Test.Sanity.BlogPost" [
      testCase "showColor"    test_showColor
    , testCase "toJSON"       test_toJSON
    , testCase "parseConfig"  test_parseConfig
    , testCase "recordToJSON" test_recordToJSON
    ]

test_showColor :: Assertion
test_showColor =
    assertEqual "" expected $
      show magenta
  where
    expected :: String
    expected = "Record {red = 1.0, green = 0.0, blue = 1.0}"

test_toJSON :: Assertion
test_toJSON =
    assertEqual "" expected $
      Aeson.Pretty.encodePretty' aesonPrettyConfig $
        toJSON magenta
  where
    expected :: Lazy.ByteString
    expected = Lazy.BS.C8.intercalate "\n" [
          "{"
        , "    \"red\": 1,"
        , "    \"green\": 0,"
        , "    \"blue\": 1"
        , "}"
        ]

test_parseConfig :: Assertion
test_parseConfig = do
    assertEqual "in order" expected . first show $
      runParser parseConfig ["2.1", "14", "Example"]
    assertEqual "out of order" expected . first show $
      runParser parseConfig' ["Example", "2.1", "14"]
    assertEqual "out of order, WRONG" expected' . first show $
      runParser parseConfigWRONG ["Example", "2.1", "14"]
  where
    expected :: Either String (Record Config)
    expected = Right ANON {
          margin   = 2.1
        , fontSize = 14
        , header   = "Example"
        }

    expected' :: Either String (Record Config)
    expected' = Left "(line 1, column 1):\nunexpected Example"

test_recordToJSON :: Assertion
test_recordToJSON = do
    assertEqual "" expected $
      Aeson.Pretty.encodePretty' aesonPrettyConfig $
        recordToJSON
          defaultFieldToJSON{header = FieldToJSON headerToJSON}
          defaultConfig
  where
    expected :: Lazy.ByteString
    expected = Lazy.BS.C8.intercalate "\n" [
          "{"
        , "    \"margin\": 1,"
        , "    \"fontSize\": 18,"
        , "    \"header\": null"
        , "}"
        ]

    headerToJSON :: String -> Value
    headerToJSON "" = Aeson.Null
    headerToJSON xs = toJSON xs

aesonPrettyConfig :: Aeson.Pretty.Config
aesonPrettyConfig = Aeson.Pretty.defConfig {
      Aeson.Pretty.confCompare = compare `on` (fieldIndex . Text.unpack)
    }
  where
    -- This is pretty hacky, but it just makes the examples a bit nicer and
    -- avoids compatibility problems between Aeson versions.
    fieldIndex :: String -> Int
    -- Color
    fieldIndex "red"   = 0
    fieldIndex "green" = 1
    fieldIndex "blue"  = 2
    -- Config
    fieldIndex "margin"   = 0
    fieldIndex "fontSize" = 1
    fieldIndex "header"   = 2
    -- Other
    fieldIndex name = error $ "fieldIndex: unknown field " ++ show name

{-------------------------------------------------------------------------------
  Introduction example

  Introduces ANON syntax, and field get/set
-------------------------------------------------------------------------------}

magenta :: Record [ "red" := Double, "green" := Double, "blue" := Double ]
magenta = ANON { red = 1, green = 0, blue = 1 }

reduceRed :: RowHasField "red" r Double => Record r -> Record r
reduceRed c = c{red = c.red * 0.9}

{-------------------------------------------------------------------------------
  The simple API

  - Alternative syntax for writing records
  - Alternative ways to access fields
    o set/get
    o lens
  - Show and other standard instances
    Show the signature of `showRecord`, forward reference to discussion
    about `KnownFields` and `AllFields`.
  - Generics (example: ToJSON)
  - Subrows
-------------------------------------------------------------------------------}

purple :: Record [ "red" := Double, "green" := Double, "blue" := Double ]
purple =
     S.insert #red   0.5
   $ S.insert #green 0
   $ S.insert #blue  0.5
   $ S.empty

reduceGreen :: RowHasField "green" r Double => Record r -> Record r
reduceGreen c = S.set #green (S.get #green c * 0.9) c

reduceBlue :: RowHasField "blue" r Double => Record r -> Record r
reduceBlue = over #blue (* 0.9)

showRecord :: (KnownFields r, AllFields r Show) => Record r -> String
showRecord = show

type Config = [
      "margin"   := Double
    , "fontSize" := Int
    , "header"   := String
    ]

defaultConfig :: Record Config
defaultConfig = ANON {
      margin   = 1
    , fontSize = 18
    , header   = ""
    }

render :: Record Config -> ()
render = undefined

exampleRender :: ()
exampleRender = render $ defaultConfig{margin = 2}

render' :: SubRow Config overrides => Record overrides -> ()
render' overrides = render (S.inject overrides defaultConfig)

exampleRender' :: ()
exampleRender' = render' $ ANON { margin = 2 }

{-------------------------------------------------------------------------------
  Advanced API

  - Functor argument
  - Usefulness of ordering
-------------------------------------------------------------------------------}

magenta' :: A.Record I [ "red" := Double, "green" := Double, "blue" := Double ]
magenta' = S.toAdvanced magenta

parseConfig :: Parser (Record Config)
parseConfig = S.sequenceA $ ANON_F {
      margin   = parseDouble
    , fontSize = parseInt
    , header   = parseString
    }

parseConfig' :: Parser (Record Config)
parseConfig' = fmap S.project . S.sequenceA $ ANON_F {
      header   = parseString
    , margin   = parseDouble
    , fontSize = parseInt
    }

parseConfigWRONG :: Parser (Record Config)
parseConfigWRONG = S.sequenceA . A.project $ ANON_F {
      header   = parseString
    , margin   = parseDouble
    , fontSize = parseInt
    }

{-------------------------------------------------------------------------------
  Slightly more elaborate example: generate JSON with per-field overrides
-------------------------------------------------------------------------------}

newtype FieldToJSON a = FieldToJSON (a -> Value)

recordToJSON :: KnownFields r => A.Record FieldToJSON r -> Record r -> Value
recordToJSON fs xs = Aeson.object . map (first fromString) $
    A.toList $ A.zipWith aux fs (S.toAdvanced xs)
  where
    aux :: FieldToJSON x -> I x -> K Value x
    aux (FieldToJSON f) (I x) = K (f x)

defaultFieldToJSON :: AllFields r ToJSON => A.Record FieldToJSON r
defaultFieldToJSON = A.cpure (Proxy @ToJSON) (FieldToJSON toJSON)

newtype NamedFieldToJSON a = NamedFieldToJSON (String -> a -> Value)

recordToJSON' :: forall r.
     KnownFields r
  => A.Record NamedFieldToJSON r -> Record r -> Value
recordToJSON' fs xs = Aeson.object . map (first fromString) $
    A.toList $
             A.pure (fn_3 aux)
      `A.ap` fs
      `A.ap` A.reifyKnownFields (Proxy @r)
      `A.ap` S.toAdvanced xs
  where
    aux :: NamedFieldToJSON x -> K String x -> I x -> K Value x
    aux (NamedFieldToJSON f) (K name) (I x) = K (f name x)

{-------------------------------------------------------------------------------
  Examples of proving constraints

  No induction, so we must do something different.
-------------------------------------------------------------------------------}

ordImpliesEq :: AllFields r Ord => Reflected (AllFields r Eq)
ordImpliesEq =
    A.reflectAllFields $
      A.map aux (A.reifyAllFields (Proxy @Ord))
  where
    aux :: forall x. Dict Ord x -> Dict Eq x
    aux Dict = Dict

smallerSatisfies :: forall r r' c.
     (SubRow r r', AllFields r c)
  => Proxy c -> Proxy r -> Reflected (AllFields r' c)
smallerSatisfies pc _ =
    A.reflectAllFields $
      A.project (A.reifyAllFields pc :: A.Record (Dict c) r)

{-------------------------------------------------------------------------------
  Advanced example: check for projection

  We avoid Typeable, just to show that we can. The version in
  "Test.Infra.Discovery" gives better error messages and is kind-polymorphic.
-------------------------------------------------------------------------------}

data SupportedType a where
  SupportedInt  :: SupportedType Int
  SupportedBool :: SupportedType Bool

class IsSupportedType a where
  supportedType :: Proxy a -> SupportedType a

instance IsSupportedType Int  where supportedType _ = SupportedInt
instance IsSupportedType Bool where supportedType _ = SupportedBool

sameType :: SupportedType a -> SupportedType b -> Maybe (a :~: b)
sameType SupportedInt  SupportedInt  = Just Refl
sameType SupportedBool SupportedBool = Just Refl
sameType _             _             = Nothing

checkIsSubRow :: forall (r :: Row Type) (r' :: Row Type) proxy proxy'.
     ( KnownFields r
     , KnownFields r'
     , SubRow r  r
     , SubRow r' r'
     , AllFields r  IsSupportedType
     , AllFields r' IsSupportedType
     )
  => proxy r -> proxy' r' -> Maybe (Reflected (SubRow r r'))
checkIsSubRow _ _ =
    A.reflectSubRow <$> go A.reifySubRow A.reifySubRow
  where
    go :: A.Record (InRow r ) r
       -> A.Record (InRow r') r'
       -> Maybe (A.Record (InRow r) r')
    go r r' = A.cmapM (Proxy @IsSupportedType) (findField r) r'

    findField :: forall x'.
          IsSupportedType x'
       => A.Record (InRow r) r -> InRow r' x' -> Maybe (InRow r x')
    findField r x' =
        listToMaybe . catMaybes . A.collapse $
          A.cmap (Proxy @IsSupportedType) (checkIsMatch x') r

    checkIsMatch :: forall x x'.
         (IsSupportedType x, IsSupportedType x')
      => InRow r' x' -> InRow r x -> K (Maybe (InRow r x')) x
    checkIsMatch (InRow x') (InRow x) = K $ do
        Refl <- sameSymbol x x'
        Refl <- sameType (supportedType (Proxy @x)) (supportedType (Proxy @x'))
        return $ InRow x

{-------------------------------------------------------------------------------
  Auxiliary: simple Parsec instantation just for demonstration purposes
-------------------------------------------------------------------------------}

type Parser a = Parsec [String] () a

runParser :: Parser a -> [String] -> Either ParseError a
runParser p = Parsec.runParser p () ""

parser :: Read a => Parser a
parser = Parsec.tokenPrim id (\pos _ _ -> pos) readMaybe

parseDouble :: Parser Double
parseDouble = parser

parseInt :: Parser Int
parseInt = parser

parseString :: Parser String
parseString = Parsec.tokenPrim id (\pos _ _ -> pos) Just
