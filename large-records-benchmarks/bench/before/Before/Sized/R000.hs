{-# OPTIONS_GHC -fplugin=RecordDotPreprocessor #-}

#if PROFILE_CORESIZE
{-# OPTIONS_GHC -ddump-to-file -ddump-ds-preopt -ddump-ds -ddump-simpl #-}
#endif
#if PROFILE_TIMING
{-# OPTIONS_GHC -ddump-to-file -ddump-timings #-}
#endif

-- | Empty module, to establish a baseline for memory usage
--
-- For fairness, we still include the plugin.
module Before.Sized.R000 () where

