module Main (main) where

#if PROFILE_RUNTIME

import Criterion.Main

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
    ]

#else

main :: IO ()
main = return ()

#endif