module CheckPtTests.MediaCollectionTests (mediaCollectionTests) where

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

import qualified CheckPt.MediaCollection as MC
import qualified CheckPt.MediaItem as MI

mediaCollectionTests = [group1,
                        group2,
                        group3,
                        group4,
                        group5,
                        group6,
                        group7,
                        group8,
                        group9]

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
                str = "Foos\n  [\ESC[32mX\ESC[0m] Foo1\n  [ ] Foo2"

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


group5 = testGroup "CheckPt uncomplete" [test9, test10]

test9 = testCase "Does nothing to an MC with no items" $ MC.uncomplete base_mc @?= base_mc

test10 = testCase "Marks all items as incomplete" $ (MC.items $ MC.uncomplete mc) @?= [mi3,mi2]
          where mc  = MC.MediaCollection { MC.name = "Foos", MC.items = [mi1, mi2] }
                mi1 = MI.MediaItem { MI.name = "Foo1", MI.completed = True }
                mi2 = MI.MediaItem { MI.name = "Foo2", MI.completed = False }
                mi3 = MI.MediaItem { MI.name = "Foo1", MI.completed = False }

group6 = testGroup "CheckPt unclearItems" [test11, test12]

test11 = testCase "Does nothing if given an empty name list" $ MC.unclearItems mc [] @?= mc
          where mc  = MC.MediaCollection { MC.name = "Foos", MC.items = [mi1, mi2] }

test12 = testCase "Only marks the select names as incomplete" $ (MC.items $ MC.unclearItems mc ["Foo1"]) @?= [mi3, mi2]
          where mc  = MC.MediaCollection { MC.name = "Foos", MC.items = [mi1, mi2] }
                mi3 = MI.MediaItem { MI.name = "Foo1", MI.completed = False }

group7 = testGroup "CheckPt deleteItems" [test13, test14]

test13 = testCase "Does nothing if given an empty name list" $ MC.deleteItems mc [] @?= mc
          where mc  = MC.MediaCollection { MC.name = "Foos", MC.items = [mi1, mi2] }

test14 = testCase "Only deletes the select names" $ (MC.items $ MC.deleteItems mc ["Foo2"]) @?= [mi1]
          where mc  = MC.MediaCollection { MC.name = "Foos", MC.items = [mi1, mi2] }

group8 = testGroup "CheckPt completed" [test15, test16, test17]

test15 = testCase "Returns True on an empty collection" $ MC.completed base_mc @?= True

test16 = testCase "Returns True on a collection with all completed items" $ MC.completed mc @?= True
          where mc = MC.MediaCollection { MC.name = "Foos", MC.items = [mi1, mi1]}

test17 = testCase "Returns False on a collection with all incomplete items" $ MC.completed mc @?= False
          where mc = MC.MediaCollection { MC.name = "Foos", MC.items = [mi2, mi2]}

group9 = testGroup "CheckPt garbageCollect" [test18, test19, test20]

test18 = testCase "Does nothing on an empty MediaCollection" $ MC.garbageCollect base_mc @?= base_mc

test19 = testCase "Does nothing to collections with all completed items" $ MC.garbageCollect mc @?= mc
          where mc = MC.MediaCollection { MC.name = "Foos", MC.items = [mi2, mi2]}

test20 = testCase "Removes completed items" $ MC.garbageCollect mc1 @?= mc2
          where mc1 = MC.MediaCollection { MC.name = "Foos", MC.items = [mi1, mi2]}
                mc2 = MC.MediaCollection { MC.name = "Foos", MC.items = [mi2]}
