module MediaCollectionTests where

import MediaCollection
import Test.HUnit

--FIXME: real tests
test1 = TestCase (assertEqual "math is hard," 3 (1 + 2))

tests = TestList [TestLabel "Trivial Addition" test1]
