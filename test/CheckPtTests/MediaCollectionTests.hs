module CheckPtTests.MediaCollectionTests (mediaCollectionTests) where

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

import qualified CheckPt.MediaCollection as MC
import qualified CheckPt.MediaItem as MI

mediaCollectionTests = [group1, group2, group3, group4]

-- Fixtures
mi1 = MI.MediaItem { MI.name = "Foo1", MI.completed = True }
mi2 = MI.MediaItem { MI.name = "Foo2", MI.completed = False }
base_mc = MC.MediaCollection { MC.name = "Foos", MC.items = [] }

group1 = testGroup "CheckPt Push" [test1, test2]

test1 = testCase "Adds to empty items list" $ (MC.items $ MC.push base_mc mi1) @?= [mi1]

test2 = testCase "Prepends to non-empty items list" $ head (MC.items $ MC.push mc mi2) @?= mi2
          where mc  = MC.MediaCollection { MC.name = "Foos", MC.items = [mi1] }

group2 = testGroup "CheckPt show" [test3, test4]

test3 = testCase "Just prints the name for an empty collection" $ show base_mc @?= str
          where str = "Foos"

test4 = testCase "Prints the name and intented items" $ show mc @?= str
          where mc  = MC.MediaCollection { MC.name = "Foos", MC.items = [mi1, mi2] }
                str = "Foos\n\t[X] Foo1\n\t[ ] Foo2"

group3 = testGroup "CheckPt complete" [test5, test6]

test5 = testCase "Does nothing to an MC with no items" $ MC.complete base_mc @?= base_mc

test6 = testCase "Marks all items as completed" $ (MC.items $ MC.complete mc) @?= [mi1,mi3]
          where mc  = MC.MediaCollection { MC.name = "Foos", MC.items = [mi1, mi2] }
                mi1 = MI.MediaItem { MI.name = "Foo1", MI.completed = True }
                mi2 = MI.MediaItem { MI.name = "Foo2", MI.completed = False }
                mi3 = MI.MediaItem { MI.name = "Foo2", MI.completed = True }

group4 = testGroup "CheckPt clearitems" [test7, test8]

test7 = testCase "Does nothing if given an empty name list" $ MC.clearItems mc [] @?= mc
          where mc  = MC.MediaCollection { MC.name = "Foos", MC.items = [mi1, mi2] }

test8 = testCase "Only marks the select names as complete" $ (MC.items $ MC.clearItems mc ["Foo2"]) @?= [mi1, mi3]
          where mc  = MC.MediaCollection { MC.name = "Foos", MC.items = [mi1, mi2] }
                mi3 = MI.MediaItem { MI.name = "Foo2", MI.completed = True }
