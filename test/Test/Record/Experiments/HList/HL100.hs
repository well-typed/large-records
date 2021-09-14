{-# LANGUAGE CPP       #-}
{-# LANGUAGE DataKinds #-}

#ifdef USE_GHC_DUMP
{-# OPTIONS_GHC -fplugin GhcDump.Plugin #-}
#endif

module Test.Record.Experiments.HList.HL100 where

import Test.Record.Experiments.HList
import Test.Record.Size.Infra

type ExampleFields = '[
      -- 0
      T 00
    , T 01
    , T 02
    , T 03
    , T 04
    , T 05
    , T 06
    , T 07
    , T 08
    , T 09
      -- 1
    , T 10
    , T 11
    , T 12
    , T 13
    , T 14
    , T 15
    , T 16
    , T 17
    , T 18
    , T 19
      -- 2
    , T 20
    , T 21
    , T 22
    , T 23
    , T 24
    , T 25
    , T 26
    , T 27
    , T 28
    , T 29
      -- 3
    , T 30
    , T 31
    , T 32
    , T 33
    , T 34
    , T 35
    , T 36
    , T 37
    , T 38
    , T 39
      -- 4
    , T 40
    , T 41
    , T 42
    , T 43
    , T 44
    , T 45
    , T 46
    , T 47
    , T 48
    , T 49
      -- 5
    , T 50
    , T 51
    , T 52
    , T 53
    , T 54
    , T 55
    , T 56
    , T 57
    , T 58
    , T 59
      -- 6
    , T 60
    , T 61
    , T 62
    , T 63
    , T 64
    , T 65
    , T 66
    , T 67
    , T 68
    , T 69
      -- 7
    , T 70
    , T 71
    , T 72
    , T 73
    , T 74
    , T 75
    , T 76
    , T 77
    , T 78
    , T 79
      -- 8
    , T 80
    , T 81
    , T 82
    , T 83
    , T 84
    , T 85
    , T 86
    , T 87
    , T 88
    , T 89
      -- 9
    , T 90
    , T 91
    , T 92
    , T 93
    , T 94
    , T 95
    , T 96
    , T 97
    , T 98
    , T 99
    ]

exampleValue :: HList ExampleFields
exampleValue =
       -- 0
       MkT 00
    :* MkT 01
    :* MkT 02
    :* MkT 03
    :* MkT 04
    :* MkT 05
    :* MkT 06
    :* MkT 07
    :* MkT 08
    :* MkT 09
       -- 1
    :* MkT 10
    :* MkT 11
    :* MkT 12
    :* MkT 13
    :* MkT 14
    :* MkT 15
    :* MkT 16
    :* MkT 17
    :* MkT 18
    :* MkT 19
       -- 2
    :* MkT 20
    :* MkT 21
    :* MkT 22
    :* MkT 23
    :* MkT 24
    :* MkT 25
    :* MkT 26
    :* MkT 27
    :* MkT 28
    :* MkT 29
       -- 3
    :* MkT 30
    :* MkT 31
    :* MkT 32
    :* MkT 33
    :* MkT 34
    :* MkT 35
    :* MkT 36
    :* MkT 37
    :* MkT 38
    :* MkT 39
       -- 4
    :* MkT 40
    :* MkT 41
    :* MkT 42
    :* MkT 43
    :* MkT 44
    :* MkT 45
    :* MkT 46
    :* MkT 47
    :* MkT 48
    :* MkT 49
       -- 5
    :* MkT 50
    :* MkT 51
    :* MkT 52
    :* MkT 53
    :* MkT 54
    :* MkT 55
    :* MkT 56
    :* MkT 57
    :* MkT 58
    :* MkT 59
       -- 6
    :* MkT 60
    :* MkT 61
    :* MkT 62
    :* MkT 63
    :* MkT 64
    :* MkT 65
    :* MkT 66
    :* MkT 67
    :* MkT 68
    :* MkT 69
       -- 7
    :* MkT 70
    :* MkT 71
    :* MkT 72
    :* MkT 73
    :* MkT 74
    :* MkT 75
    :* MkT 76
    :* MkT 77
    :* MkT 78
    :* MkT 79
       -- 8
    :* MkT 80
    :* MkT 81
    :* MkT 82
    :* MkT 83
    :* MkT 84
    :* MkT 85
    :* MkT 86
    :* MkT 87
    :* MkT 88
    :* MkT 89
       -- 9
    :* MkT 90
    :* MkT 91
    :* MkT 92
    :* MkT 93
    :* MkT 94
    :* MkT 95
    :* MkT 96
    :* MkT 97
    :* MkT 98
    :* MkT 99
       --
    :* Nil
