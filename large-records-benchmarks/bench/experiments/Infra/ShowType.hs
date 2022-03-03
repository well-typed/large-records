{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies   #-}

module Infra.ShowType (ShowType) where

import Data.Kind
import GHC.TypeLits

import Bench.Types (T)

{-------------------------------------------------------------------------------
  Showing types

  This isn't very general obviously but it's just for an example.
-------------------------------------------------------------------------------}

type family ShowType (a :: Type) :: Symbol

type instance ShowType Int    = "Int"
type instance ShowType Char   = "Char"
type instance ShowType [Char] = "String"
type instance ShowType Double = "Double"
type instance ShowType Bool   = "Bool"

-- We want to avoid unnecessary coercions to do with computation of names
-- so we just hardcode stuff.

-- 0
type instance ShowType (T 00) = "T 00"
type instance ShowType (T 01) = "T 01"
type instance ShowType (T 02) = "T 02"
type instance ShowType (T 03) = "T 03"
type instance ShowType (T 04) = "T 04"
type instance ShowType (T 05) = "T 05"
type instance ShowType (T 06) = "T 06"
type instance ShowType (T 07) = "T 07"
type instance ShowType (T 08) = "T 08"
type instance ShowType (T 09) = "T 09"
-- 1
type instance ShowType (T 10) = "T 10"
type instance ShowType (T 11) = "T 11"
type instance ShowType (T 12) = "T 12"
type instance ShowType (T 13) = "T 13"
type instance ShowType (T 14) = "T 14"
type instance ShowType (T 15) = "T 15"
type instance ShowType (T 16) = "T 16"
type instance ShowType (T 17) = "T 17"
type instance ShowType (T 18) = "T 18"
type instance ShowType (T 19) = "T 19"
-- 2
type instance ShowType (T 20) = "T 20"
type instance ShowType (T 21) = "T 21"
type instance ShowType (T 22) = "T 22"
type instance ShowType (T 23) = "T 23"
type instance ShowType (T 24) = "T 24"
type instance ShowType (T 25) = "T 25"
type instance ShowType (T 26) = "T 26"
type instance ShowType (T 27) = "T 27"
type instance ShowType (T 28) = "T 28"
type instance ShowType (T 29) = "T 29"
-- 3
type instance ShowType (T 30) = "T 30"
type instance ShowType (T 31) = "T 31"
type instance ShowType (T 32) = "T 32"
type instance ShowType (T 33) = "T 33"
type instance ShowType (T 34) = "T 34"
type instance ShowType (T 35) = "T 35"
type instance ShowType (T 36) = "T 36"
type instance ShowType (T 37) = "T 37"
type instance ShowType (T 38) = "T 38"
type instance ShowType (T 39) = "T 39"
-- 4
type instance ShowType (T 40) = "T 40"
type instance ShowType (T 41) = "T 41"
type instance ShowType (T 42) = "T 42"
type instance ShowType (T 43) = "T 43"
type instance ShowType (T 44) = "T 44"
type instance ShowType (T 45) = "T 45"
type instance ShowType (T 46) = "T 46"
type instance ShowType (T 47) = "T 47"
type instance ShowType (T 48) = "T 48"
type instance ShowType (T 49) = "T 49"
-- 5
type instance ShowType (T 50) = "T 50"
type instance ShowType (T 51) = "T 51"
type instance ShowType (T 52) = "T 52"
type instance ShowType (T 53) = "T 53"
type instance ShowType (T 54) = "T 54"
type instance ShowType (T 55) = "T 55"
type instance ShowType (T 56) = "T 56"
type instance ShowType (T 57) = "T 57"
type instance ShowType (T 58) = "T 58"
type instance ShowType (T 59) = "T 59"
-- 6
type instance ShowType (T 60) = "T 60"
type instance ShowType (T 61) = "T 61"
type instance ShowType (T 62) = "T 62"
type instance ShowType (T 63) = "T 63"
type instance ShowType (T 64) = "T 64"
type instance ShowType (T 65) = "T 65"
type instance ShowType (T 66) = "T 66"
type instance ShowType (T 67) = "T 67"
type instance ShowType (T 68) = "T 68"
type instance ShowType (T 69) = "T 69"
-- 7
type instance ShowType (T 70) = "T 70"
type instance ShowType (T 71) = "T 71"
type instance ShowType (T 72) = "T 72"
type instance ShowType (T 73) = "T 73"
type instance ShowType (T 74) = "T 74"
type instance ShowType (T 75) = "T 75"
type instance ShowType (T 76) = "T 76"
type instance ShowType (T 77) = "T 77"
type instance ShowType (T 78) = "T 78"
type instance ShowType (T 79) = "T 79"
-- 8
type instance ShowType (T 80) = "T 80"
type instance ShowType (T 81) = "T 81"
type instance ShowType (T 82) = "T 82"
type instance ShowType (T 83) = "T 83"
type instance ShowType (T 84) = "T 84"
type instance ShowType (T 85) = "T 85"
type instance ShowType (T 86) = "T 86"
type instance ShowType (T 87) = "T 87"
type instance ShowType (T 88) = "T 88"
type instance ShowType (T 89) = "T 89"
-- 9
type instance ShowType (T 90) = "T 90"
type instance ShowType (T 91) = "T 91"
type instance ShowType (T 92) = "T 92"
type instance ShowType (T 93) = "T 93"
type instance ShowType (T 94) = "T 94"
type instance ShowType (T 95) = "T 95"
type instance ShowType (T 96) = "T 96"
type instance ShowType (T 97) = "T 97"
type instance ShowType (T 98) = "T 98"
type instance ShowType (T 99) = "T 99"

