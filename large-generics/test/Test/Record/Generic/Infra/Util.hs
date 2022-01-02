module Test.Record.Generic.Infra.Util (
    expectException
  ) where

import Control.Exception
import Test.Tasty.HUnit

-- | Only used internally in 'expectException'
data Result =
    NoException
  | ExpectedException
  | UnexpectedException SomeException

expectException :: (SomeException -> Bool) -> Assertion -> Assertion
expectException p k = do
    result <- handle (return . aux) (k >> return NoException)
    case result of
      ExpectedException ->
        return ()
      NoException ->
        assertFailure $ "Expected exception, but none was raised"
      UnexpectedException e ->
        assertFailure $ "Raised exception does not match predicate: " ++ show e
  where
    aux :: SomeException -> Result
    aux e | p e       = ExpectedException
          | otherwise = UnexpectedException e

