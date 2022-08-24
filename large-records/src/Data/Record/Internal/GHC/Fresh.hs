{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Record.Internal.GHC.Fresh (
    MonadFresh(..)
  , runFreshHsc
  ) where

import Data.IORef
import Control.Monad.Reader

import Data.Record.Internal.GHC.Shim

class Monad m => MonadFresh m where
  -- | Construct a fresh name for use in term level expressions
  --
  -- NOTES:
  --
  -- * These names should be used for module exports.
  -- * These names should be used for exactly /one/ binder.
  -- * The resulting name has the same 'NameSpace' as the argument.
  freshName :: LRdrName -> m LRdrName
  freshName = freshName' True

  -- variant which doesn't rename the variable.
  -- The 'False' variant can be used in types.
  freshName' :: Bool -> LRdrName -> m LRdrName

newtype Fresh a = WrapFresh { unwrapFresh :: ReaderT (IORef NameCache) IO a }
  deriving newtype (Functor, Applicative, Monad)

instance MonadFresh Fresh where
  freshName' pfx (L l name) = WrapFresh $ ReaderT $ \nc_var ->
      atomicModifyIORef nc_var aux
    where
      aux :: NameCache -> (NameCache, LRdrName)
      aux nc = (
            nc { nsUniqs = us }
          , L l $ Exact $
              mkInternalName newUniq (newOccName (rdrNameOcc name)) l
          )
        where
          (newUniq, us) = takeUniqFromSupply (nsUniqs nc)

      -- Even when we generate fresh names, ghc can still complain about name
      -- shadowing, because this check only considers the 'OccName', not the
      -- unique. We therefore prefix the name with an underscore to avoid the
      -- warning.
      newOccName :: OccName -> OccName
      newOccName n = mkOccName (occNameSpace n) $ addPrefix $ occNameString n

      addPrefix :: String -> String
      addPrefix = if pfx then ("_" ++) else id

runFresh :: Fresh a -> IORef NameCache -> IO a
runFresh = runReaderT . unwrapFresh

runFreshHsc :: Fresh a -> Hsc a
runFreshHsc fa = do
    env <- getHscEnv
    liftIO $ runFresh fa (hsc_NC env)


