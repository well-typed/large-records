{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Naming things is hard
module Data.Record.Anonymous.SrcPlugin.NamingT (
    -- * Monad definition
    NamingT -- opaque
  , runNamingT
  , runNamingHsc
    -- * Key features of NamingT
  , useName
  , fresh
    -- * Convenience derived features
  , freshVar
  ) where

import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor
import Data.IORef
import Data.Set (Set)

import qualified Data.Set as Set

import Data.Record.Anonymous.SrcPlugin.GhcShim

{-------------------------------------------------------------------------------
  Monad definition
-------------------------------------------------------------------------------}

data Env = Env {
      envNameCache :: IORef NameCache
    }

-- | Naming things is hard
--
-- The 'NamingT' monad transformer that provides two things:
--
-- 1. Keep track of the imports we need for the names that we use.
-- 2. Generation of fresh names.
newtype NamingT m a = WrapNamingT {
      unwrapNamingT :: StateT (Set ModuleName) (ReaderT Env m) a
    }
  deriving (Functor, Applicative, Monad)

instance MonadTrans NamingT where
  lift = WrapNamingT . lift . lift

runNamingT :: Functor m => IORef NameCache -> NamingT m a -> m (a, [ModuleName])
runNamingT ncVar =
      fmap (second Set.toList)
    . flip runReaderT env
    . flip runStateT  Set.empty
    . unwrapNamingT
  where
    env :: Env
    env = Env { envNameCache = ncVar }

runNamingHsc :: NamingT Hsc a -> Hsc (a, [ModuleName])
runNamingHsc ma = do
    env <- getHscEnv
    runNamingT (hsc_NC env) ma

{-------------------------------------------------------------------------------
  Key features of NamingT
-------------------------------------------------------------------------------}

useName :: Monad m => RdrName -> NamingT m ()
useName (Qual modl _) = WrapNamingT $ modify (Set.insert modl)
useName _otherwise    = error "useName: expected qualified name"

fresh :: MonadIO m => SrcSpan -> RdrName -> NamingT m RdrName
fresh l name = WrapNamingT $ do
    ncVar <- asks envNameCache
    liftIO $ atomicModifyIORef ncVar aux
  where
    aux :: NameCache -> (NameCache, RdrName)
    aux nc = (
          nc { nsUniqs = us }
        , Exact $ mkInternalName newUniq (newOccName (rdrNameOcc name)) l
        )
      where
        (newUniq, us) = takeUniqFromSupply (nsUniqs nc)

    -- Even when we generate fresh names, ghc can still complain about name
    -- shadowing, because this check only considers the 'OccName', not the
    -- unique. We therefore prefix the name with an underscore to avoid the
    -- warning.
    newOccName :: OccName -> OccName
    newOccName n = mkOccName (occNameSpace n) . ("_" ++) $ occNameString n

{-------------------------------------------------------------------------------
  Derived convenience functions
-------------------------------------------------------------------------------}

freshVar :: MonadIO m => SrcSpan -> String -> NamingT m RdrName
freshVar l = fresh l . mkRdrUnqual . mkVarOcc
