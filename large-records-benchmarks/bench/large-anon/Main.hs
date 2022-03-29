{-# LANGUAGE BangPatterns #-}

module Main (main) where

#if PROFILE_RUNTIME

import Bench.Util.Criterion
import Criterion.Main

import Data.Record.Anonymous.Simple (applyDiff)

import qualified Bench.EvensOfSize.Evens010
import qualified Bench.EvensOfSize.Evens020
import qualified Bench.EvensOfSize.Evens030
import qualified Bench.EvensOfSize.Evens040
import qualified Bench.EvensOfSize.Evens050
import qualified Bench.EvensOfSize.Evens060
import qualified Bench.EvensOfSize.Evens070
import qualified Bench.EvensOfSize.Evens080
import qualified Bench.EvensOfSize.Evens090
import qualified Bench.EvensOfSize.Evens100

import qualified Experiment.ConstructNoTypeLet.Sized.R010
import qualified Experiment.ConstructNoTypeLet.Sized.R020
import qualified Experiment.ConstructNoTypeLet.Sized.R030
import qualified Experiment.ConstructNoTypeLet.Sized.R040
import qualified Experiment.ConstructNoTypeLet.Sized.R050
import qualified Experiment.ConstructNoTypeLet.Sized.R060
import qualified Experiment.ConstructNoTypeLet.Sized.R070
import qualified Experiment.ConstructNoTypeLet.Sized.R080
import qualified Experiment.ConstructNoTypeLet.Sized.R090
import qualified Experiment.ConstructNoTypeLet.Sized.R100

import qualified Experiment.ConstructNoApply.Sized.R010
import qualified Experiment.ConstructNoApply.Sized.R020
import qualified Experiment.ConstructNoApply.Sized.R030
import qualified Experiment.ConstructNoApply.Sized.R040
import qualified Experiment.ConstructNoApply.Sized.R050
import qualified Experiment.ConstructNoApply.Sized.R060
import qualified Experiment.ConstructNoApply.Sized.R070
import qualified Experiment.ConstructNoApply.Sized.R080
import qualified Experiment.ConstructNoApply.Sized.R090
import qualified Experiment.ConstructNoApply.Sized.R100

import qualified Experiment.ConstructWithTypeLet.Sized.R010
import qualified Experiment.ConstructWithTypeLet.Sized.R020
import qualified Experiment.ConstructWithTypeLet.Sized.R030
import qualified Experiment.ConstructWithTypeLet.Sized.R040
import qualified Experiment.ConstructWithTypeLet.Sized.R050
import qualified Experiment.ConstructWithTypeLet.Sized.R060
import qualified Experiment.ConstructWithTypeLet.Sized.R070
import qualified Experiment.ConstructWithTypeLet.Sized.R080
import qualified Experiment.ConstructWithTypeLet.Sized.R090
import qualified Experiment.ConstructWithTypeLet.Sized.R100

import qualified Experiment.GetEvens.Sized.R010
import qualified Experiment.GetEvens.Sized.R020
import qualified Experiment.GetEvens.Sized.R030
import qualified Experiment.GetEvens.Sized.R040
import qualified Experiment.GetEvens.Sized.R050
import qualified Experiment.GetEvens.Sized.R060
import qualified Experiment.GetEvens.Sized.R070
import qualified Experiment.GetEvens.Sized.R080
import qualified Experiment.GetEvens.Sized.R090
import qualified Experiment.GetEvens.Sized.R100

import qualified Experiment.SetEvens.Sized.R010
import qualified Experiment.SetEvens.Sized.R020
import qualified Experiment.SetEvens.Sized.R030
import qualified Experiment.SetEvens.Sized.R040
import qualified Experiment.SetEvens.Sized.R050
import qualified Experiment.SetEvens.Sized.R060
import qualified Experiment.SetEvens.Sized.R070
import qualified Experiment.SetEvens.Sized.R080
import qualified Experiment.SetEvens.Sized.R090
import qualified Experiment.SetEvens.Sized.R100

import qualified Experiment.ToJSON.Sized.R010
import qualified Experiment.ToJSON.Sized.R020
import qualified Experiment.ToJSON.Sized.R030
import qualified Experiment.ToJSON.Sized.R040
import qualified Experiment.ToJSON.Sized.R050
import qualified Experiment.ToJSON.Sized.R060
import qualified Experiment.ToJSON.Sized.R070
import qualified Experiment.ToJSON.Sized.R080
import qualified Experiment.ToJSON.Sized.R090
import qualified Experiment.ToJSON.Sized.R100

import qualified Experiment.ParseJSON.Sized.R010
import qualified Experiment.ParseJSON.Sized.R020
import qualified Experiment.ParseJSON.Sized.R030
import qualified Experiment.ParseJSON.Sized.R040
import qualified Experiment.ParseJSON.Sized.R050
import qualified Experiment.ParseJSON.Sized.R060
import qualified Experiment.ParseJSON.Sized.R070
import qualified Experiment.ParseJSON.Sized.R080
import qualified Experiment.ParseJSON.Sized.R090
import qualified Experiment.ParseJSON.Sized.R100

main :: IO ()
main = defaultMain [
      bgroup "ConstructNoTypeLet" [
          bench "010" $ whnf Experiment.ConstructNoTypeLet.Sized.R010.record 0
        , bench "020" $ whnf Experiment.ConstructNoTypeLet.Sized.R020.record 0
        , bench "030" $ whnf Experiment.ConstructNoTypeLet.Sized.R030.record 0
        , bench "040" $ whnf Experiment.ConstructNoTypeLet.Sized.R040.record 0
        , bench "050" $ whnf Experiment.ConstructNoTypeLet.Sized.R050.record 0
        , bench "060" $ whnf Experiment.ConstructNoTypeLet.Sized.R060.record 0
        , bench "070" $ whnf Experiment.ConstructNoTypeLet.Sized.R070.record 0
        , bench "080" $ whnf Experiment.ConstructNoTypeLet.Sized.R080.record 0
        , bench "090" $ whnf Experiment.ConstructNoTypeLet.Sized.R090.record 0
        , bench "100" $ whnf Experiment.ConstructNoTypeLet.Sized.R100.record 0
        ]
    , bgroup "ConstructNoApply" [
          bench "010" $ whnf Experiment.ConstructNoApply.Sized.R010.record 0
        , bench "020" $ whnf Experiment.ConstructNoApply.Sized.R020.record 0
        , bench "030" $ whnf Experiment.ConstructNoApply.Sized.R030.record 0
        , bench "040" $ whnf Experiment.ConstructNoApply.Sized.R040.record 0
        , bench "050" $ whnf Experiment.ConstructNoApply.Sized.R050.record 0
        , bench "060" $ whnf Experiment.ConstructNoApply.Sized.R060.record 0
        , bench "070" $ whnf Experiment.ConstructNoApply.Sized.R070.record 0
        , bench "080" $ whnf Experiment.ConstructNoApply.Sized.R080.record 0
        , bench "090" $ whnf Experiment.ConstructNoApply.Sized.R090.record 0
        , bench "100" $ whnf Experiment.ConstructNoApply.Sized.R100.record 0
        ]
    , bgroup "ConstructWithTypeLet" [
          bench "010" $ whnf Experiment.ConstructWithTypeLet.Sized.R010.record 0
        , bench "020" $ whnf Experiment.ConstructWithTypeLet.Sized.R020.record 0
        , bench "030" $ whnf Experiment.ConstructWithTypeLet.Sized.R030.record 0
        , bench "040" $ whnf Experiment.ConstructWithTypeLet.Sized.R040.record 0
        , bench "050" $ whnf Experiment.ConstructWithTypeLet.Sized.R050.record 0
        , bench "060" $ whnf Experiment.ConstructWithTypeLet.Sized.R060.record 0
        , bench "070" $ whnf Experiment.ConstructWithTypeLet.Sized.R070.record 0
        , bench "080" $ whnf Experiment.ConstructWithTypeLet.Sized.R080.record 0
        , bench "090" $ whnf Experiment.ConstructWithTypeLet.Sized.R090.record 0
        , bench "100" $ whnf Experiment.ConstructWithTypeLet.Sized.R100.record 0
        ]
    , bgroup "GetEvensAfterApply" [
          envPureWHNF (Experiment.ConstructNoTypeLet.Sized.R010.record 0) $ \r ->
            bench "010" $ whnf Experiment.GetEvens.Sized.R010.getEvens r
        , envPureWHNF (Experiment.ConstructNoTypeLet.Sized.R020.record 0) $ \r ->
            bench "020" $ whnf Experiment.GetEvens.Sized.R020.getEvens r
        , envPureWHNF (Experiment.ConstructNoTypeLet.Sized.R030.record 0) $ \r ->
            bench "030" $ whnf Experiment.GetEvens.Sized.R030.getEvens r
        , envPureWHNF (Experiment.ConstructNoTypeLet.Sized.R040.record 0) $ \r ->
            bench "040" $ whnf Experiment.GetEvens.Sized.R040.getEvens r
        , envPureWHNF (Experiment.ConstructNoTypeLet.Sized.R050.record 0) $ \r ->
            bench "050" $ whnf Experiment.GetEvens.Sized.R050.getEvens r
        , envPureWHNF (Experiment.ConstructNoTypeLet.Sized.R060.record 0) $ \r ->
            bench "060" $ whnf Experiment.GetEvens.Sized.R060.getEvens r
        , envPureWHNF (Experiment.ConstructNoTypeLet.Sized.R070.record 0) $ \r ->
            bench "070" $ whnf Experiment.GetEvens.Sized.R070.getEvens r
        , envPureWHNF (Experiment.ConstructNoTypeLet.Sized.R080.record 0) $ \r ->
            bench "080" $ whnf Experiment.GetEvens.Sized.R080.getEvens r
        , envPureWHNF (Experiment.ConstructNoTypeLet.Sized.R090.record 0) $ \r ->
            bench "090" $ whnf Experiment.GetEvens.Sized.R090.getEvens r
        , envPureWHNF (Experiment.ConstructNoTypeLet.Sized.R100.record 0) $ \r ->
            bench "100" $ whnf Experiment.GetEvens.Sized.R100.getEvens r
        ]
    , bgroup "GetEvensNoApply" [
          envPureWHNF (Experiment.ConstructNoApply.Sized.R010.record 0) $ \r ->
            bench "010" $ whnf Experiment.GetEvens.Sized.R010.getEvens r
        , envPureWHNF (Experiment.ConstructNoApply.Sized.R020.record 0) $ \r ->
            bench "020" $ whnf Experiment.GetEvens.Sized.R020.getEvens r
        , envPureWHNF (Experiment.ConstructNoApply.Sized.R030.record 0) $ \r ->
            bench "030" $ whnf Experiment.GetEvens.Sized.R030.getEvens r
        , envPureWHNF (Experiment.ConstructNoApply.Sized.R040.record 0) $ \r ->
            bench "040" $ whnf Experiment.GetEvens.Sized.R040.getEvens r
        , envPureWHNF (Experiment.ConstructNoApply.Sized.R050.record 0) $ \r ->
            bench "050" $ whnf Experiment.GetEvens.Sized.R050.getEvens r
        , envPureWHNF (Experiment.ConstructNoApply.Sized.R060.record 0) $ \r ->
            bench "060" $ whnf Experiment.GetEvens.Sized.R060.getEvens r
        , envPureWHNF (Experiment.ConstructNoApply.Sized.R070.record 0) $ \r ->
            bench "070" $ whnf Experiment.GetEvens.Sized.R070.getEvens r
        , envPureWHNF (Experiment.ConstructNoApply.Sized.R080.record 0) $ \r ->
            bench "080" $ whnf Experiment.GetEvens.Sized.R080.getEvens r
        , envPureWHNF (Experiment.ConstructNoApply.Sized.R090.record 0) $ \r ->
            bench "090" $ whnf Experiment.GetEvens.Sized.R090.getEvens r
        , envPureWHNF (Experiment.ConstructNoApply.Sized.R100.record 0) $ \r ->
            bench "100" $ whnf Experiment.GetEvens.Sized.R100.getEvens r
        ]
    , bgroup "SetEvensThenApply" [
          envPureWHNF (Experiment.ConstructNoTypeLet.Sized.R010.record 0) $ \r ->
            bench "010" $ whnf (applyDiff . Experiment.SetEvens.Sized.R010.setEvens evens010) r
        , envPureWHNF (Experiment.ConstructNoTypeLet.Sized.R020.record 0) $ \r ->
            bench "020" $ whnf (applyDiff . Experiment.SetEvens.Sized.R020.setEvens evens020) r
        , envPureWHNF (Experiment.ConstructNoTypeLet.Sized.R030.record 0) $ \r ->
            bench "030" $ whnf (applyDiff . Experiment.SetEvens.Sized.R030.setEvens evens030) r
        , envPureWHNF (Experiment.ConstructNoTypeLet.Sized.R040.record 0) $ \r ->
            bench "040" $ whnf (applyDiff . Experiment.SetEvens.Sized.R040.setEvens evens040) r
        , envPureWHNF (Experiment.ConstructNoTypeLet.Sized.R050.record 0) $ \r ->
            bench "050" $ whnf (applyDiff . Experiment.SetEvens.Sized.R050.setEvens evens050) r
        , envPureWHNF (Experiment.ConstructNoTypeLet.Sized.R060.record 0) $ \r ->
            bench "060" $ whnf (applyDiff . Experiment.SetEvens.Sized.R060.setEvens evens060) r
        , envPureWHNF (Experiment.ConstructNoTypeLet.Sized.R070.record 0) $ \r ->
            bench "070" $ whnf (applyDiff . Experiment.SetEvens.Sized.R070.setEvens evens070) r
        , envPureWHNF (Experiment.ConstructNoTypeLet.Sized.R080.record 0) $ \r ->
            bench "080" $ whnf (applyDiff . Experiment.SetEvens.Sized.R080.setEvens evens080) r
        , envPureWHNF (Experiment.ConstructNoTypeLet.Sized.R090.record 0) $ \r ->
            bench "090" $ whnf (applyDiff . Experiment.SetEvens.Sized.R090.setEvens evens090) r
        , envPureWHNF (Experiment.ConstructNoTypeLet.Sized.R100.record 0) $ \r ->
            bench "100" $ whnf (applyDiff . Experiment.SetEvens.Sized.R100.setEvens evens100) r
        ]
    , bgroup "SetEvensNoApply" [
          envPureWHNF (Experiment.ConstructNoTypeLet.Sized.R010.record 0) $ \r ->
            bench "010" $ whnf (Experiment.SetEvens.Sized.R010.setEvens evens010) r
        , envPureWHNF (Experiment.ConstructNoTypeLet.Sized.R020.record 0) $ \r ->
            bench "020" $ whnf (Experiment.SetEvens.Sized.R020.setEvens evens020) r
        , envPureWHNF (Experiment.ConstructNoTypeLet.Sized.R030.record 0) $ \r ->
            bench "030" $ whnf (Experiment.SetEvens.Sized.R030.setEvens evens030) r
        , envPureWHNF (Experiment.ConstructNoTypeLet.Sized.R040.record 0) $ \r ->
            bench "040" $ whnf (Experiment.SetEvens.Sized.R040.setEvens evens040) r
        , envPureWHNF (Experiment.ConstructNoTypeLet.Sized.R050.record 0) $ \r ->
            bench "050" $ whnf (Experiment.SetEvens.Sized.R050.setEvens evens050) r
        , envPureWHNF (Experiment.ConstructNoTypeLet.Sized.R060.record 0) $ \r ->
            bench "060" $ whnf (Experiment.SetEvens.Sized.R060.setEvens evens060) r
        , envPureWHNF (Experiment.ConstructNoTypeLet.Sized.R070.record 0) $ \r ->
            bench "070" $ whnf (Experiment.SetEvens.Sized.R070.setEvens evens070) r
        , envPureWHNF (Experiment.ConstructNoTypeLet.Sized.R080.record 0) $ \r ->
            bench "080" $ whnf (Experiment.SetEvens.Sized.R080.setEvens evens080) r
        , envPureWHNF (Experiment.ConstructNoTypeLet.Sized.R090.record 0) $ \r ->
            bench "090" $ whnf (Experiment.SetEvens.Sized.R090.setEvens evens090) r
        , envPureWHNF (Experiment.ConstructNoTypeLet.Sized.R100.record 0) $ \r ->
            bench "100" $ whnf (Experiment.SetEvens.Sized.R100.setEvens evens100) r
        ]
    , bgroup "ToJSON" [
          envPureWHNF (Experiment.ConstructNoTypeLet.Sized.R010.record 0) $ \r ->
            bench "010" $ nf Experiment.ToJSON.Sized.R010.recToJSON r
        , envPureWHNF (Experiment.ConstructNoTypeLet.Sized.R020.record 0) $ \r ->
            bench "020" $ nf Experiment.ToJSON.Sized.R020.recToJSON r
        , envPureWHNF (Experiment.ConstructNoTypeLet.Sized.R030.record 0) $ \r ->
            bench "030" $ nf Experiment.ToJSON.Sized.R030.recToJSON r
        , envPureWHNF (Experiment.ConstructNoTypeLet.Sized.R040.record 0) $ \r ->
            bench "040" $ nf Experiment.ToJSON.Sized.R040.recToJSON r
        , envPureWHNF (Experiment.ConstructNoTypeLet.Sized.R050.record 0) $ \r ->
            bench "050" $ nf Experiment.ToJSON.Sized.R050.recToJSON r
        , envPureWHNF (Experiment.ConstructNoTypeLet.Sized.R060.record 0) $ \r ->
            bench "060" $ nf Experiment.ToJSON.Sized.R060.recToJSON r
        , envPureWHNF (Experiment.ConstructNoTypeLet.Sized.R070.record 0) $ \r ->
            bench "070" $ nf Experiment.ToJSON.Sized.R070.recToJSON r
        , envPureWHNF (Experiment.ConstructNoTypeLet.Sized.R080.record 0) $ \r ->
            bench "080" $ nf Experiment.ToJSON.Sized.R080.recToJSON r
        , envPureWHNF (Experiment.ConstructNoTypeLet.Sized.R090.record 0) $ \r ->
            bench "090" $ nf Experiment.ToJSON.Sized.R090.recToJSON r
        , envPureWHNF (Experiment.ConstructNoTypeLet.Sized.R100.record 0) $ \r ->
            bench "100" $ nf Experiment.ToJSON.Sized.R100.recToJSON r
        ]
    , bgroup "ParseJSON" [
          envPureNF (Experiment.ToJSON.Sized.R010.recToJSON $ Experiment.ConstructNoTypeLet.Sized.R010.record 0) $ \r ->
            bench "010" $ whnf Experiment.ParseJSON.Sized.R010.recFromJSON r
        , envPureNF (Experiment.ToJSON.Sized.R020.recToJSON $ Experiment.ConstructNoTypeLet.Sized.R020.record 0) $ \r ->
            bench "020" $ whnf Experiment.ParseJSON.Sized.R020.recFromJSON r
        , envPureNF (Experiment.ToJSON.Sized.R030.recToJSON $ Experiment.ConstructNoTypeLet.Sized.R030.record 0) $ \r ->
            bench "030" $ whnf Experiment.ParseJSON.Sized.R030.recFromJSON r
        , envPureNF (Experiment.ToJSON.Sized.R040.recToJSON $ Experiment.ConstructNoTypeLet.Sized.R040.record 0) $ \r ->
            bench "040" $ whnf Experiment.ParseJSON.Sized.R040.recFromJSON r
        , envPureNF (Experiment.ToJSON.Sized.R050.recToJSON $ Experiment.ConstructNoTypeLet.Sized.R050.record 0) $ \r ->
            bench "050" $ whnf Experiment.ParseJSON.Sized.R050.recFromJSON r
        , envPureNF (Experiment.ToJSON.Sized.R060.recToJSON $ Experiment.ConstructNoTypeLet.Sized.R060.record 0) $ \r ->
            bench "060" $ whnf Experiment.ParseJSON.Sized.R060.recFromJSON r
        , envPureNF (Experiment.ToJSON.Sized.R070.recToJSON $ Experiment.ConstructNoTypeLet.Sized.R070.record 0) $ \r ->
            bench "070" $ whnf Experiment.ParseJSON.Sized.R070.recFromJSON r
        , envPureNF (Experiment.ToJSON.Sized.R080.recToJSON $ Experiment.ConstructNoTypeLet.Sized.R080.record 0) $ \r ->
            bench "080" $ whnf Experiment.ParseJSON.Sized.R080.recFromJSON r
        , envPureNF (Experiment.ToJSON.Sized.R090.recToJSON $ Experiment.ConstructNoTypeLet.Sized.R090.record 0) $ \r ->
            bench "090" $ whnf Experiment.ParseJSON.Sized.R090.recFromJSON r
        , envPureNF (Experiment.ToJSON.Sized.R100.recToJSON $ Experiment.ConstructNoTypeLet.Sized.R100.record 0) $ \r ->
            bench "100" $ whnf Experiment.ParseJSON.Sized.R100.recFromJSON r
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
    evens090 :: Bench.EvensOfSize.Evens090.Evens
    evens100 :: Bench.EvensOfSize.Evens100.Evens

    !evens010 = Bench.EvensOfSize.Evens010.evens
    !evens020 = Bench.EvensOfSize.Evens020.evens
    !evens030 = Bench.EvensOfSize.Evens030.evens
    !evens040 = Bench.EvensOfSize.Evens040.evens
    !evens050 = Bench.EvensOfSize.Evens050.evens
    !evens060 = Bench.EvensOfSize.Evens060.evens
    !evens070 = Bench.EvensOfSize.Evens070.evens
    !evens080 = Bench.EvensOfSize.Evens080.evens
    !evens090 = Bench.EvensOfSize.Evens090.evens
    !evens100 = Bench.EvensOfSize.Evens100.evens

#else

main :: IO ()
main = return ()

#endif