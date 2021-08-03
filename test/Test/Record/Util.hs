{-# LANGUAGE DeriveLift                 #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE TemplateHaskell            #-}

module Test.Record.Util (
    -- * Tasty/HUnit auxiliary
    assertPrefix
  , assertJust
    -- * TH error collection
  , Problem(..)
  , CollectProblems -- opaque
  , collectProblems
  , collectOnlyProblems
  ) where

import Control.Monad.Except hiding (lift)
import Control.Monad.State hiding (lift)
import Data.Bifunctor
import Data.List (isPrefixOf)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Test.Tasty.HUnit

import qualified Control.Monad.Trans.Class as Trans

{-------------------------------------------------------------------------------
  Tasty/HUnit auxiliary
-------------------------------------------------------------------------------}

assertPrefix :: (Show a, Eq a) => [a] -> [a] -> Assertion
assertPrefix xs ys =
   assertBool (show xs ++ " is not a prefix of " ++ show ys) $
      xs `isPrefixOf` ys

assertJust :: String -> Maybe a -> (a -> Assertion) -> Assertion
assertJust msg Nothing  _ = assertFailure msg
assertJust _   (Just a) k = k a

{-------------------------------------------------------------------------------
  TH error collection

  We use explicit error collection so that we can /test/ that we are
  generating the appropriate errors and warnings.
-------------------------------------------------------------------------------}

data Problem = Error String | Warning String
  deriving (Show, Eq, Lift)

problemIsError :: Problem -> Bool
problemIsError (Error   _) = True
problemIsError (Warning _) = False

-- | 'Quasi' monad which allows us to inspect errors and warnings
--
-- We use this to /test/ that our TH code is reporting the expected errors
-- and warnings.
newtype CollectProblems a = CollectProblems {
      -- Implementation notes:
      --
      -- o 'ExceptT' for 'MonadFail', 'StateT' for 'qReport'
      -- o 'ExceptT' on the outside, so that /if/ there is a failure (as in
      --   'fail'), we still get the errors/warnings that were reported
      --   before the failure.
      -- o Problems are stored in reverse order
      -- o Errors and warnings are stored together, rather than as two lists,
      --   so that we preserve their relative ordering.
      unwrapCollectProblems :: ExceptT String (StateT [Problem] Q) a
    }
  deriving newtype (Functor, Applicative, Monad, MonadIO)

liftQ :: Q a -> CollectProblems a
liftQ = CollectProblems . Trans.lift . Trans.lift

instance MonadFail CollectProblems where
  fail str = CollectProblems $ throwError str

-- | Run 'CollectProblems'
collectProblems :: CollectProblems a -> Q (Either String a, [Problem])
collectProblems = fmap (second reverse) . runCollectProblems

-- | Like 'collectProblems', but return /only/ the problems
collectOnlyProblems :: CollectProblems a -> Q (Maybe String, [Problem])
collectOnlyProblems = fmap (first aux) . collectProblems
  where
    aux :: Either String a -> Maybe String
    aux = either Just (const Nothing)

-- | Lower-level version of 'runCollectProblems' that reports errors in reverse
-- order
runCollectProblems :: CollectProblems a -> Q (Either String a, [Problem])
runCollectProblems = (`runStateT` []) . runExceptT  . unwrapCollectProblems

instance Quasi CollectProblems where
  -- Only error-reporting is different

  qReport :: Bool -> String -> CollectProblems ()
  qReport False w = CollectProblems $ modify (Warning w:)
  qReport True  e = CollectProblems $ modify (Error   e:)

  qRecover :: CollectProblems a -> CollectProblems a -> CollectProblems a
  qRecover handler action =
      CollectProblems $ ExceptT $ StateT $ \problemsBeforeRecovery -> do
        -- We run the action with an empty list of problems; this is okay,
        -- because there is no way to /inspect/ the current list of problems,
        -- we only catch problems thrown inside the action
        (mResult, problemsDuringAction) <- runCollectProblems action
        case mResult of
          Right a | not (any problemIsError problemsDuringAction) ->
            return (Right a, problemsDuringAction ++ problemsBeforeRecovery)
          _otherwise -> do
            -- If there was a failure, _or_ an error (but not a warning), the
            -- handler runs. In this case, any errors /or/ warnings that were
            -- reported during the action are ignored.
            second (++ problemsBeforeRecovery) <$> runCollectProblems handler

  -- Everything else just lifts

  qNewName            = \x   -> liftQ $ qNewName            x
  qLookupName         = \x y -> liftQ $ qLookupName         x y
  qReify              = \x   -> liftQ $ qReify              x
  qReifyFixity        = \x   -> liftQ $ qReifyFixity        x
  qReifyInstances     = \x y -> liftQ $ qReifyInstances     x y
  qReifyRoles         = \x   -> liftQ $ qReifyRoles         x
  qReifyAnnotations   = \x   -> liftQ $ qReifyAnnotations   x
  qReifyModule        = \x   -> liftQ $ qReifyModule        x
  qReifyConStrictness = \x   -> liftQ $ qReifyConStrictness x
  qLocation           =         liftQ $ qLocation
  qAddDependentFile   = \x   -> liftQ $ qAddDependentFile   x
  qAddTempFile        = \x   -> liftQ $ qAddTempFile        x
  qAddTopDecls        = \x   -> liftQ $ qAddTopDecls        x
  qAddForeignFilePath = \x y -> liftQ $ qAddForeignFilePath x y
  qAddModFinalizer    = \x   -> liftQ $ qAddModFinalizer    x
  qAddCorePlugin      = \x   -> liftQ $ qAddCorePlugin      x
  qGetQ               =         liftQ $ qGetQ
  qPutQ               = \x   -> liftQ $ qPutQ               x
  qIsExtEnabled       = \x   -> liftQ $ qIsExtEnabled       x
  qExtsEnabled        =         liftQ $ qExtsEnabled

{-------------------------------------------------------------------------------
  Internal auxiliary: figure out how TH does error handling
-------------------------------------------------------------------------------}

-- | TH error behaviour
--
-- TH error behaviour is a bit of a mess. When the code below is executed, it
-- yields:
--
-- > n == 1
-- > a
-- > n == 2
-- > handler_2
-- > n == 3
-- > handler_3
-- > n == 4
-- > handler_4
-- > n == 4
--
-- Some observations:
--
-- 1. The documentation of `recover` says that it allows recovery from errors
--    raised by `reportError` or `fail`. The above supports this, /but/ notice
--    that if code raises errors that are caught, any warnings thrown by that
--    code /also/ disappear.
-- 2. The documentation of `qRecover` only talks about recovery from `fail`,
--    not `reportError`, but `recover` is just a wrapper around `qRecover`.
-- 3. Unlike `fail`, `reportError` does not halt execution. This means that
--    even though we can attempt to recover from errors raised by `reportError`,
--    we actually have no idea what the state looks like when recovery begins.
-- 4. Despite `IO` having a `Quasi` instance, `recover` actually does not work
--    /at all/ in `IO` (in fact, /most/ functions of the 'Quasi' class are not
--    supported in IO).
-- 5. Who knew that TH had dynamically typed global variables?
_thErrorBehaviour :: Q [Dec]
_thErrorBehaviour = do
    qPutQ (1 :: Int)
    showN

    -- What happens if we /only/ throw a warning?
    recover (reportWarning "handler_1" >> return ()) $ do
      reportWarning "a"
      qPutQ (2 :: Int)

    showN

    -- What happens if we throw warnings /around/ an error?
    recover (reportWarning "handler_2" >> return ()) $ do
      reportWarning "b"
      reportError   "c"
      reportWarning "d"
      qPutQ (3 :: Int)

    showN

    -- What happens if we throw /multiple/ errors?
    recover (reportWarning "handler_3" >> return ()) $ do
      reportError "e"
      reportError "f"
      qPutQ (4 :: Int)

    showN

    -- What happens if we /fail/ after a warning?
    recover (reportWarning "handler_4" >> return ()) $ do
      reportWarning "g"
      () <- fail "h"
      qPutQ (5 :: Int)

    showN
    return []
  where
    showN :: Q ()
    showN = do
        Just n <- qGetQ
        reportWarning $ "n == " ++ show (n :: Int)

