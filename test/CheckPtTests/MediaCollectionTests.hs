module CheckPtTests.MediaCollectionTests (mediaCollectionTests) where

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

import qualified CheckPt.MediaCollection as MC
import qualified CheckPt.MediaItem as MI

mediaCollectionTests = [group1, group2]

-- Fixtures

group1 = testGroup "CheckPt Push" [test1, test2]
mi1 = MI.MediaItem { MI.name = "Foo1", MI.completed = True }
mi2 = MI.MediaItem { MI.name = "Foo2", MI.completed = False }

test1 = testCase "Adds to empty items list" $ (MC.items $ MC.push mc mi1) @?= [mi1]
          where mc = MC.MediaCollection { MC.name = "Foos", MC.items = [] }

test2 = testCase "Prepends to non-empty items list" $ head (MC.items $ MC.push mc mi2) @?= mi2
          where mc  = MC.MediaCollection { MC.name = "Foos", MC.items = [mi1] }

group2 = testGroup "CheckPt show" [test3, test4]

test3 = testCase "Just prints the name for an empty collection" $ show mc @?= str
          where mc  = MC.MediaCollection { MC.name = "Foos", MC.items = [] }
                str = "Foos"

test4 = testCase "Prints the name and intented items" $ show mc @?= str
          where mc  = MC.MediaCollection { MC.name = "Foos", MC.items = [mi1, mi2] }
                str = "Foos\n\t[X] Foo1\n\t[ ] Foo2"
