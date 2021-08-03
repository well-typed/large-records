{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

-- {-# OPTIONS_GHC -ddump-splices #-}

module Test.Record.Sanity.ErrorsAndWarnings.Stage1 (
    -- * The record we're testing against
    --
    -- It is important that we export 'R', because the various TH expressions
    -- we not executed here, but rather where we use them
    -- ("Test.Record.Sanity.ErrorsAndWarnings.Stage1"); this means that if we
    -- don't export this, the " problem " that is reported is the constructor
    -- not being in scope, rather than the missing/extra fields.
    R(..)

    -- The specific tests
  , qProblemsForSyntaxErrorInExp
  , qProblemsForSyntaxErrorInPat
  , qProblemsForUnknownFieldsInExp
  , qProblemsForUnknownFieldsInPat
  , qProblemsForMissingFieldsInExp
  , qProblemsForMissingFieldsInPat
  ) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Data.Record.TH
import Data.Record.QQ.CodeGen

import Test.Record.Util

largeRecord defaultPureScript [d|
      data R = MkR { a :: Int, b :: Bool }
    |]

endOfBindingGroup

qProblemsForSyntaxErrorInExp :: Q Exp
qProblemsForSyntaxErrorInExp = (lift =<<) $ collectOnlyProblems $
    lrExp "MkR { a = 1, b = True, c = ()"

qProblemsForSyntaxErrorInPat :: Q Exp
qProblemsForSyntaxErrorInPat = (lift =<<) $ collectOnlyProblems $
    lrPat "MkR { a = x, b = y, c = z"

qProblemsForUnknownFieldsInExp :: Q Exp
qProblemsForUnknownFieldsInExp = (lift =<<) $ collectOnlyProblems $
    lrExp "MkR { a = 1, b = True, c = () }"

qProblemsForUnknownFieldsInPat :: Q Exp
qProblemsForUnknownFieldsInPat = (lift =<<) $ collectOnlyProblems $
    lrPat "MkR { a = x, b = y, c = z }"

qProblemsForMissingFieldsInExp :: Q Exp
qProblemsForMissingFieldsInExp = (lift =<<) $ collectOnlyProblems $
    lrExp "MkR { a = 1 }"

qProblemsForMissingFieldsInPat :: Q Exp
qProblemsForMissingFieldsInPat = (lift =<<) $ collectOnlyProblems $
    lrPat "MkR { a = x }"

