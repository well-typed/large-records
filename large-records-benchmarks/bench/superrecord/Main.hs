{-# LANGUAGE BangPatterns #-}

module Main (main) where

#if PROFILE_RUNTIME

import Criterion.Main
import Bench.Util.Criterion

import qualified Bench.EvensOfSize.Evens010
import qualified Bench.EvensOfSize.Evens020
import qualified Bench.EvensOfSize.Evens030
import qualified Bench.EvensOfSize.Evens040
import qualified Bench.EvensOfSize.Evens050
import qualified Bench.EvensOfSize.Evens060
import qualified Bench.EvensOfSize.Evens070
import qualified Bench.EvensOfSize.Evens080

import qualified Experiment.SR_Construct.Sized.R010
import qualified Experiment.SR_Construct.Sized.R020
import qualified Experiment.SR_Construct.Sized.R030
import qualified Experiment.SR_Construct.Sized.R040

import qualified Experiment.SR_Unsafe.Sized.R010
import qualified Experiment.SR_Unsafe.Sized.R020
import qualified Experiment.SR_Unsafe.Sized.R030
import qualified Experiment.SR_Unsafe.Sized.R040
import qualified Experiment.SR_Unsafe.Sized.R050
import qualified Experiment.SR_Unsafe.Sized.R060
import qualified Experiment.SR_Unsafe.Sized.R070
import qualified Experiment.SR_Unsafe.Sized.R080

import qualified Experiment.SR_GetEvens.Sized.R010
import qualified Experiment.SR_GetEvens.Sized.R020
import qualified Experiment.SR_GetEvens.Sized.R030
import qualified Experiment.SR_GetEvens.Sized.R040
import qualified Experiment.SR_GetEvens.Sized.R050
import qualified Experiment.SR_GetEvens.Sized.R060
import qualified Experiment.SR_GetEvens.Sized.R070
import qualified Experiment.SR_GetEvens.Sized.R080

import qualified Experiment.SR_SetEvens.Sized.R010
import qualified Experiment.SR_SetEvens.Sized.R020
import qualified Experiment.SR_SetEvens.Sized.R030
import qualified Experiment.SR_SetEvens.Sized.R040
import qualified Experiment.SR_SetEvens.Sized.R050
import qualified Experiment.SR_SetEvens.Sized.R060
import qualified Experiment.SR_SetEvens.Sized.R070
import qualified Experiment.SR_SetEvens.Sized.R080

import qualified Experiment.SR_UpdateOne.Sized.R010
import qualified Experiment.SR_UpdateOne.Sized.R020
import qualified Experiment.SR_UpdateOne.Sized.R030
import qualified Experiment.SR_UpdateOne.Sized.R040
import qualified Experiment.SR_UpdateOne.Sized.R050
import qualified Experiment.SR_UpdateOne.Sized.R060
import qualified Experiment.SR_UpdateOne.Sized.R070
import qualified Experiment.SR_UpdateOne.Sized.R080

import qualified Experiment.SR_ToJSON.Sized.R010
import qualified Experiment.SR_ToJSON.Sized.R020
import qualified Experiment.SR_ToJSON.Sized.R030
import qualified Experiment.SR_ToJSON.Sized.R040
import qualified Experiment.SR_ToJSON.Sized.R050
import qualified Experiment.SR_ToJSON.Sized.R060
import qualified Experiment.SR_ToJSON.Sized.R070
import qualified Experiment.SR_ToJSON.Sized.R080

import qualified Experiment.SR_ParseJSON.Sized.R010
import qualified Experiment.SR_ParseJSON.Sized.R020
import qualified Experiment.SR_ParseJSON.Sized.R030
import qualified Experiment.SR_ParseJSON.Sized.R040
import qualified Experiment.SR_ParseJSON.Sized.R050
import qualified Experiment.SR_ParseJSON.Sized.R060
import qualified Experiment.SR_ParseJSON.Sized.R070
import qualified Experiment.SR_ParseJSON.Sized.R080

main :: IO ()
main = defaultMain [
      bgroup "SR_Construct" [
          bench "010" $ whnf Experiment.SR_Construct.Sized.R010.record 0
        , bench "020" $ whnf Experiment.SR_Construct.Sized.R020.record 0
        , bench "030" $ whnf Experiment.SR_Construct.Sized.R030.record 0
        , bench "040" $ whnf Experiment.SR_Construct.Sized.R040.record 0
        ]
    , bgroup "SR_Unsafe" [
          bench "010" $ whnf Experiment.SR_Unsafe.Sized.R010.record 0
        , bench "020" $ whnf Experiment.SR_Unsafe.Sized.R020.record 0
        , bench "030" $ whnf Experiment.SR_Unsafe.Sized.R030.record 0
        , bench "040" $ whnf Experiment.SR_Unsafe.Sized.R040.record 0
        , bench "050" $ whnf Experiment.SR_Unsafe.Sized.R050.record 0
        , bench "060" $ whnf Experiment.SR_Unsafe.Sized.R060.record 0
        , bench "070" $ whnf Experiment.SR_Unsafe.Sized.R070.record 0
        , bench "080" $ whnf Experiment.SR_Unsafe.Sized.R080.record 0
        ]
    , bgroup "SR_GetEvens" [
          envPureWHNF (Experiment.SR_Unsafe.Sized.R010.record 0) $ \r ->
            bench "010" $ whnf Experiment.SR_GetEvens.Sized.R010.getEvens r
        , envPureWHNF (Experiment.SR_Unsafe.Sized.R020.record 0) $ \r ->
            bench "020" $ whnf Experiment.SR_GetEvens.Sized.R020.getEvens r
        , envPureWHNF (Experiment.SR_Unsafe.Sized.R030.record 0) $ \r ->
            bench "030" $ whnf Experiment.SR_GetEvens.Sized.R030.getEvens r
        , envPureWHNF (Experiment.SR_Unsafe.Sized.R040.record 0) $ \r ->
            bench "040" $ whnf Experiment.SR_GetEvens.Sized.R040.getEvens r
        , envPureWHNF (Experiment.SR_Unsafe.Sized.R050.record 0) $ \r ->
            bench "050" $ whnf Experiment.SR_GetEvens.Sized.R050.getEvens r
        , envPureWHNF (Experiment.SR_Unsafe.Sized.R060.record 0) $ \r ->
            bench "060" $ whnf Experiment.SR_GetEvens.Sized.R060.getEvens r
        , envPureWHNF (Experiment.SR_Unsafe.Sized.R070.record 0) $ \r ->
            bench "070" $ whnf Experiment.SR_GetEvens.Sized.R070.getEvens r
        , envPureWHNF (Experiment.SR_Unsafe.Sized.R080.record 0) $ \r ->
            bench "080" $ whnf Experiment.SR_GetEvens.Sized.R080.getEvens r
        ]
    , bgroup "SR_SetEvens" [
          envPureWHNF (Experiment.SR_Unsafe.Sized.R010.record 0) $ \r ->
            bench "010" $ whnf (Experiment.SR_SetEvens.Sized.R010.setEvens evens010) r
        , envPureWHNF (Experiment.SR_Unsafe.Sized.R020.record 0) $ \r ->
            bench "020" $ whnf (Experiment.SR_SetEvens.Sized.R020.setEvens evens020) r
        , envPureWHNF (Experiment.SR_Unsafe.Sized.R030.record 0) $ \r ->
            bench "030" $ whnf (Experiment.SR_SetEvens.Sized.R030.setEvens evens030) r
        , envPureWHNF (Experiment.SR_Unsafe.Sized.R040.record 0) $ \r ->
            bench "040" $ whnf (Experiment.SR_SetEvens.Sized.R040.setEvens evens040) r
        , envPureWHNF (Experiment.SR_Unsafe.Sized.R050.record 0) $ \r ->
            bench "050" $ whnf (Experiment.SR_SetEvens.Sized.R050.setEvens evens050) r
        , envPureWHNF (Experiment.SR_Unsafe.Sized.R060.record 0) $ \r ->
            bench "060" $ whnf (Experiment.SR_SetEvens.Sized.R060.setEvens evens060) r
        , envPureWHNF (Experiment.SR_Unsafe.Sized.R070.record 0) $ \r ->
            bench "070" $ whnf (Experiment.SR_SetEvens.Sized.R070.setEvens evens070) r
        , envPureWHNF (Experiment.SR_Unsafe.Sized.R080.record 0) $ \r ->
            bench "080" $ whnf (Experiment.SR_SetEvens.Sized.R080.setEvens evens080) r
        ]
    , bgroup "SR_UpdateOne" [
          envPureWHNF (Experiment.SR_Unsafe.Sized.R010.record 0) $ \r ->
            bench "010" $ whnf Experiment.SR_UpdateOne.Sized.R010.updateOne r
        , envPureWHNF (Experiment.SR_Unsafe.Sized.R020.record 0) $ \r ->
            bench "020" $ whnf Experiment.SR_UpdateOne.Sized.R020.updateOne r
        , envPureWHNF (Experiment.SR_Unsafe.Sized.R030.record 0) $ \r ->
            bench "030" $ whnf Experiment.SR_UpdateOne.Sized.R030.updateOne r
        , envPureWHNF (Experiment.SR_Unsafe.Sized.R040.record 0) $ \r ->
            bench "040" $ whnf Experiment.SR_UpdateOne.Sized.R040.updateOne r
        , envPureWHNF (Experiment.SR_Unsafe.Sized.R050.record 0) $ \r ->
            bench "050" $ whnf Experiment.SR_UpdateOne.Sized.R050.updateOne r
        , envPureWHNF (Experiment.SR_Unsafe.Sized.R060.record 0) $ \r ->
            bench "060" $ whnf Experiment.SR_UpdateOne.Sized.R060.updateOne r
        , envPureWHNF (Experiment.SR_Unsafe.Sized.R070.record 0) $ \r ->
            bench "070" $ whnf Experiment.SR_UpdateOne.Sized.R070.updateOne r
        , envPureWHNF (Experiment.SR_Unsafe.Sized.R080.record 0) $ \r ->
            bench "080" $ whnf Experiment.SR_UpdateOne.Sized.R080.updateOne r
        ]
    , bgroup "SR_ToJSON" [
          envPureWHNF (Experiment.SR_Unsafe.Sized.R010.record 0) $ \r ->
            bench "010" $ nf Experiment.SR_ToJSON.Sized.R010.recToJSON r
        , envPureWHNF (Experiment.SR_Unsafe.Sized.R020.record 0) $ \r ->
            bench "020" $ nf Experiment.SR_ToJSON.Sized.R020.recToJSON r
        , envPureWHNF (Experiment.SR_Unsafe.Sized.R030.record 0) $ \r ->
            bench "030" $ nf Experiment.SR_ToJSON.Sized.R030.recToJSON r
        , envPureWHNF (Experiment.SR_Unsafe.Sized.R040.record 0) $ \r ->
            bench "040" $ nf Experiment.SR_ToJSON.Sized.R040.recToJSON r
        , envPureWHNF (Experiment.SR_Unsafe.Sized.R050.record 0) $ \r ->
            bench "050" $ nf Experiment.SR_ToJSON.Sized.R050.recToJSON r
        , envPureWHNF (Experiment.SR_Unsafe.Sized.R060.record 0) $ \r ->
            bench "060" $ nf Experiment.SR_ToJSON.Sized.R060.recToJSON r
        , envPureWHNF (Experiment.SR_Unsafe.Sized.R070.record 0) $ \r ->
            bench "070" $ nf Experiment.SR_ToJSON.Sized.R070.recToJSON r
        , envPureWHNF (Experiment.SR_Unsafe.Sized.R080.record 0) $ \r ->
            bench "080" $ nf Experiment.SR_ToJSON.Sized.R080.recToJSON r
        ]
    , bgroup "SR_ParseJSON" [
          envPureNF (Experiment.SR_ToJSON.Sized.R010.recToJSON $ Experiment.SR_Unsafe.Sized.R010.record 0) $ \r ->
            bench "010" $ whnf Experiment.SR_ParseJSON.Sized.R010.recFromJSON r
        , envPureNF (Experiment.SR_ToJSON.Sized.R020.recToJSON $ Experiment.SR_Unsafe.Sized.R020.record 0) $ \r ->
            bench "020" $ whnf Experiment.SR_ParseJSON.Sized.R020.recFromJSON r
        , envPureNF (Experiment.SR_ToJSON.Sized.R030.recToJSON $ Experiment.SR_Unsafe.Sized.R030.record 0) $ \r ->
            bench "030" $ whnf Experiment.SR_ParseJSON.Sized.R030.recFromJSON r
        , envPureNF (Experiment.SR_ToJSON.Sized.R040.recToJSON $ Experiment.SR_Unsafe.Sized.R040.record 0) $ \r ->
            bench "040" $ whnf Experiment.SR_ParseJSON.Sized.R040.recFromJSON r
        , envPureNF (Experiment.SR_ToJSON.Sized.R050.recToJSON $ Experiment.SR_Unsafe.Sized.R050.record 0) $ \r ->
            bench "050" $ whnf Experiment.SR_ParseJSON.Sized.R050.recFromJSON r
        , envPureNF (Experiment.SR_ToJSON.Sized.R060.recToJSON $ Experiment.SR_Unsafe.Sized.R060.record 0) $ \r ->
            bench "060" $ whnf Experiment.SR_ParseJSON.Sized.R060.recFromJSON r
        , envPureNF (Experiment.SR_ToJSON.Sized.R070.recToJSON $ Experiment.SR_Unsafe.Sized.R070.record 0) $ \r ->
            bench "070" $ whnf Experiment.SR_ParseJSON.Sized.R070.recFromJSON r
        , envPureNF (Experiment.SR_ToJSON.Sized.R080.recToJSON $ Experiment.SR_Unsafe.Sized.R080.record 0) $ \r ->
            bench "080" $ whnf Experiment.SR_ParseJSON.Sized.R080.recFromJSON r
        ]
    ]
  where
    evens010 :: Bench.EvensOfSize.Evens010.Evens
    evens020 :: Bench.EvensOfSize.Evens020.Evens
    evens030 :: Bench.EvensOfSize.Evens030.Evens
    evens040 :: Bench.EvensOfSize.Evens040.Evens
    evens050 :: Bench.EvensOfSize.Evens050.Evens
    evens060 :: Bench.EvensOfSize.Evens060.Evens
    evens070 :: Bench.EvensOfSize.Evens070.Evens
    evens080 :: Bench.EvensOfSize.Evens080.Evens

    !evens010 = Bench.EvensOfSize.Evens010.evens
    !evens020 = Bench.EvensOfSize.Evens020.evens
    !evens030 = Bench.EvensOfSize.Evens030.evens
    !evens040 = Bench.EvensOfSize.Evens040.evens
    !evens050 = Bench.EvensOfSize.Evens050.evens
    !evens060 = Bench.EvensOfSize.Evens060.evens
    !evens070 = Bench.EvensOfSize.Evens070.evens
    !evens080 = Bench.EvensOfSize.Evens080.evens

#else

main :: IO ()
main = return ()

#endif