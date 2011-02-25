module CheckPtTests.DataSetTests (dataSetTests) where

import Data.List (intercalate)

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

import qualified CheckPt.DataSet as DS
import qualified CheckPt.MediaItem as MI
import qualified CheckPt.MediaCollection as MC
import qualified CheckPt.CLI.Result as R

dataSetTests = [group1, 
                group2,
                group3,
                group4,
                group5,
                group6,
                group7,
                group8,
                group9,
                group10,
                group11,
                group12,
                group13,
                group14,
                group15,
                group16,
                group17,
                group18,
                group19]

-- Fixtures (yay functional programming!)
mc1 = MC.MediaCollection { MC.name = "Foos", MC.items = []}
mc2 = MC.MediaCollection { MC.name = "Bars", MC.items = []}
mi1 = MI.MediaItem { MI.name = "Foo1", MI.completed = False }
mi2 = MI.MediaItem { MI.name = "Foo2", MI.completed = False }
mi3 = MI.MediaItem { MI.name = "Foo3", MI.completed = True }
base_ds = DS.DataSet { DS.collections = [], DS.items = [] }

fromSuccess :: R.Result DS.DataSet -> DS.DataSet
fromSuccess (R.Success d) = d

isFailure :: R.Result DS.DataSet -> Bool
isFailure (R.Failure _) = True
isFailure _ = False

group1 = testGroup "DataSet parseDataSet" [test1, test2, test3, test4]

test1 = testCase "Parses Empty Collections" $ (DS.collections $ DS.parseDataSet json) @?= []
          where json = "{\"collections\":[],\"items\":[]}"

test2 = testCase "Parses Collections" $ (DS.collections $ DS.parseDataSet json) @?= [mc1]
          where json = "{\"collections\":[{\"name\":\"Foos\",\"items\":[]}],\"items\":[]}"

test3 = testCase "Parses Empty Items" $ (DS.items $ DS.parseDataSet json) @?= []
          where json = "{\"collections\":[],\"items\":[]}"

test4 = testCase "Parses Items" $ (DS.items $ DS.parseDataSet json) @?= [mi1]
        where json = "{\"collections\":[],\"items\":[{\"name\":\"Foo1\",\"completed\":false}]}"

group2 = testGroup "DataSet datasetPath" [test5]

test5 = testCase "Appends \".checkpt\" to the path" $ DS.dataSetPath base @?= final
          where base = "/home/michael"
                final = "/home/michael/.checkpt"

group3 = testGroup "DataSet stringify" [test6, test7, test8]

test6 = testCase "Correctly serializes an empty dataset" $ DS.stringify base_ds @?= json
          where json = "{\"collections\":[],\"items\":[]}"

test7 = testCase "Serializes Collections" $ DS.stringify ds @?= json
          where ds   = DS.DataSet { DS.collections = [mc1], DS.items = [] }
                json = "{\"collections\":[{\"name\":\"Foos\",\"items\":[]}],\"items\":[]}"

test8 = testCase "Serializes Items" $ DS.stringify ds @?= json
          where ds   = DS.DataSet { DS.collections = [], DS.items = [mi1] }
                json = "{\"collections\":[],\"items\":[{\"name\":\"Foo1\",\"completed\":false}]}"


group4 = testGroup "DataSet show" [test9, test10, test11, test12]

test9 = testCase "Displays an empy string when empty" $ show base_ds @?= str
          where str = ""

test10 = testCase "Displays only collections when items empty" $ show ds @?= str
          where str = show mc1 ++ "\n" ++ show mc2
                ds  = DS.DataSet { DS.collections = [mc1, mc2], DS.items = [] }

test11 = testCase "Displays only items when collections empty" $ show ds @?= str
          where str = show mi1 ++ "\n" ++ show mi2
                ds  = DS.DataSet { DS.collections = [], DS.items = [mi1, mi2] }

test12 = testCase "Displays items first" $ show ds @?= str
          where str = show mi1 ++ "\n" ++  show mi2 ++ "\n" ++ show mc1 ++ "\n" ++ show mc2
                ds  = DS.DataSet { DS.collections = [mc1, mc2], DS.items = [mi1, mi2] }

group5 = testGroup "DataSet pushItem" [test13, test14]

test13 = testCase "Appends an item to an empty list" $ DS.items ds @?= [mi1]
          where ds = DS.pushItem base_ds mi1

test14 = testCase "Prepends an item to a non-empty list" $ head (DS.items ds) @?= mi1
          where ds = DS.pushItem (DS.DataSet {DS.collections = [], DS.items = [mi2]}) mi1

group6 = testGroup "DataSet pushCollection" [test15, test16]

test15 = testCase "Appends a collection to an empty list" $ DS.collections ds @?= [mc1]
          where ds = DS.pushCollection base_ds mc1

test16 = testCase "Prepends a collection to a non-empty list" $ head (DS.collections ds) @?= mc1
          where ds = DS.pushCollection (DS.DataSet {DS.items = [], DS.collections = [mc2]}) mc1

group7 = testGroup "DataSet lookupItem" [test17, test18]

test17 = testCase "Finds an item by name if it exists" $ DS.lookupItem ds "Foo2" @?= (Just mi2)
          where ds = DS.DataSet { DS.collections = [], DS.items = [mi1,mi2] }

test18 = testCase "Returns Nothing if it does not exist" $ DS.lookupItem ds "Bogus" @?= Nothing
          where ds = DS.DataSet { DS.collections = [], DS.items = [mi1,mi2] }

group8 = testGroup "DataSet lookupCollection" [test17, test18]

test19 = testCase "Finds a collection by name if it exists" $ DS.lookupCollection ds "Foos" @?= (Just mc2)
          where ds = DS.DataSet { DS.collections = [mc1,mc2], DS.items = [] }

test20 = testCase "Returns Nothing if it does not exist" $ DS.lookupCollection ds "Bogus" @?= Nothing
          where ds = DS.DataSet { DS.collections = [mc1,mc2], DS.items = [] }

group9 = testGroup "DataSet itemExists" [test21, test22]

test21 = testCase "Returns True if the item is found" $ DS.itemExists ds "Foo2" @?= True
          where ds = DS.DataSet { DS.collections = [], DS.items = [mi1,mi2] }

test22 = testCase "Returns False if the item is found" $ DS.itemExists ds "Bogus" @?= False
          where ds = DS.DataSet { DS.collections = [], DS.items = [mi1,mi2] }

group10 = testGroup "DataSet collectionExists" [test23, test24]

test23 = testCase "Returns True if the collection is found" $ DS.collectionExists ds "Foos" @?= True
          where ds = DS.DataSet { DS.collections = [mc1,mc2], DS.items = [] }

test24 = testCase "Returns False if the collection is found" $ DS.collectionExists ds "Bogus" @?= False
          where ds = DS.DataSet { DS.collections = [mc1,mc2], DS.items = [] }

group11 = testGroup "DataSet clearCollections" [test25, test26]

test25 = testCase "Does nothing on with empty collections" $ (fromSuccess $ DS.clearCollections base_ds) @?= base_ds

test26 = testCase "Sets all collections to completed" $ (all MI.completed $ concatMap MC.items . DS.collections . fromSuccess $ DS.clearCollections ds) @?= True
          where mc1 = MC.MediaCollection { MC.name = "Foos", MC.items = [mi1, mi2] }
                mc2 = MC.MediaCollection { MC.name = "Bars", MC.items = [mi1, mi2] }
                ds  = DS.DataSet { DS.collections = [mc1,mc2], DS.items = [] }

group12 = testGroup "DataSet clearCollection" [test27]

test27 = testCase "Sets the found collection to completed" $ (MC.items . last . DS.collections . fromSuccess $ DS.clearCollection ds "Bars") @?= [mi3,mi4]
          where mc1 = MC.MediaCollection { MC.name = "Foos", MC.items = [mi1,mi2] }
                mc2 = MC.MediaCollection { MC.name = "Bars", MC.items = [mi1,mi2] }
                mi3 = MI.MediaItem { MI.name = "Foo1", MI.completed = True}
                mi4 = MI.MediaItem { MI.name = "Foo2", MI.completed = True}
                ds  = DS.DataSet { DS.collections = [mc1,mc2], DS.items = [] }

group13 = testGroup "DataSet clearCollectionItems" [test28]

test28 = testCase "Only completes specific items in specific collections" $ (MC.items . last . DS.collections . fromSuccess $  DS.clearCollectionItems ds "Bars" ["Foo2"]) @?= [mi1,mi3]
          where mc1 = MC.MediaCollection { MC.name = "Foos", MC.items = [mi1,mi2] }
                mc2 = MC.MediaCollection { MC.name = "Bars", MC.items = [mi1,mi2] }
                mi3 = MI.MediaItem { MI.name = "Foo2", MI.completed = True}
                ds  = DS.DataSet { DS.collections = [mc1,mc2], DS.items = [] }

group14 = testGroup "DataSet clearItem" [test29]

test29 = testCase "Clears only the specified root item" $ (DS.items . fromSuccess $ DS.clearItem ds "Foo2") @?= [mi1,mi3]
          where mi3 = MI.MediaItem { MI.name = "Foo2", MI.completed = True}
                ds  = DS.DataSet { DS.collections = [], DS.items = [mi1,mi2] }

group15 = testGroup "DataSet rootNames" [test30, test31, test32]

test30 = testCase "Is an empty list with no items" $ DS.rootNames base_ds @?= []

test31 = testCase "is only the root items if no collections" $ DS.rootNames ds @?= ["\"Foo1\"", "\"Foo2\""]
          where ds = DS.DataSet { DS.collections = [], DS.items = [mi1,mi2] }

test32 = testCase "it combines item and collection names, items first" $ DS.rootNames ds @?= ["\"Foo1\"", "\"Foo2\"", "\"Foos\"", "\"Bars\""]
          where ds = DS.DataSet { DS.collections = [mc1,mc2], DS.items = [mi1,mi2] }

group16 = testGroup "DataSet collectionNames" [test33, test34, test35]

test33 = testCase "Is an empty list with no collections" $ DS.collectionNames base_ds "_" @?= []

test34 = testCase "Is an empty list with bogus collection" $ DS.collectionNames ds "Bogus" @?= []
          where ds = DS.DataSet { DS.collections = [mc1,mc2], DS.items = [] }

test35 = testCase "it is only the items under the specified collection" $ DS.collectionNames ds "Foos" @?= ["\"Foo1\"", "\"Foo2\""]
          where mc1 = MC.MediaCollection { MC.name = "Foos", MC.items = [mi1,mi2] }
                mc2 = MC.MediaCollection { MC.name = "Bars", MC.items = [mi1,mi2] }
                ds  = DS.DataSet { DS.collections = [mc1,mc2], DS.items = [] }

group17 = testGroup "DataSet deleteCollection" [test36, test37]

test36 = testCase "Does nothing with a bogus name" $ (fromSuccess $ DS.deleteCollection ds "Bogus") @?= ds
          where ds  = DS.DataSet { DS.collections = [mc1,mc2], DS.items = [] }

test37 = testCase "Only deletes the collection specified" $ ( DS.collections . fromSuccess $ DS.deleteCollection ds "Foos") @?= [mc2]
          where ds  = DS.DataSet { DS.collections = [mc1,mc2], DS.items = [] }

group18 = testGroup "DataSet deleteCollectionItems" [test38]

test38 = testCase "Only deletes specific items in specific collections" $ (MC.items . last . DS.collections . fromSuccess $ DS.deleteCollectionItems ds "Bars" ["Foo2"]) @?= [mi1]
          where mc1 = MC.MediaCollection { MC.name = "Foos", MC.items = [mi1,mi2] }
                mc2 = MC.MediaCollection { MC.name = "Bars", MC.items = [mi1,mi2] }
                ds  = DS.DataSet { DS.collections = [mc1,mc2], DS.items = [] }

group19 = testGroup "DataSet garbageCollect" [test39, test40, test41]

test39 = testCase "Does nothing on an empty DataSet" $ DS.garbageCollect base_ds @?= base_ds

test40 = testCase "Deletes completed items" $ DS.garbageCollect base_ds @?= base_ds
          where ds  = DS.DataSet { DS.collections = [], DS.items = [mi3, mi3] }

test41 = testCase "Deletes completed MediaCollections" $ DS.garbageCollect ds1 @?= ds2
          where mc1  = MC.MediaCollection { MC.name = "Foos", MC.items = [mi1,mi3] }
                mc1b = MC.MediaCollection { MC.name = "Foos", MC.items = [mi1] }
                mc2  = MC.MediaCollection { MC.name = "Bars", MC.items = [mi3,mi3] }
                ds1  = DS.DataSet { DS.collections = [mc1,mc2], DS.items = [] }
                ds2  = DS.DataSet { DS.collections = [mc1b], DS.items = [] }
