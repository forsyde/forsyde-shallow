module Shallow (shallowTest) where

import Shallow.SynchronousMoC (synchronousMoCTest)
import Test.HUnit

shallowTest :: Test
shallowTest = test [synchronousMoCTest]