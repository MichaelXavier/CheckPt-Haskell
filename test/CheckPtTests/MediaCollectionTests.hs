module CheckPtTests.MediaCollectionTests (mediaCollectionTests) where

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

import qualified CheckPt.MediaCollection as MC
import qualified CheckPt.MediaItem as MI

mediaCollectionTests = [group1]

group1 = testGroup "CheckPt Push" [test1, test2]

test1 = testCase "Adds to empty items list" $ (MC.items $ MC.push mc mi) @?= [mi]
          where mc = MC.MediaCollection { MC.name = "Foos", MC.items = [] }
                mi = MI.MediaItem { MI.name = "Foo1", MI.completed = False }

test2 = testCase "Prepends to non-empty items list" $ head (MC.items $ MC.push mc mi2) @?= mi2
          where mc  = MC.MediaCollection { MC.name = "Foos", MC.items = [mi1] }
                mi1 =  MI.MediaItem { MI.name = "Foo1", MI.completed = False }
                mi2 =  MI.MediaItem { MI.name = "Foo2", MI.completed = False }
