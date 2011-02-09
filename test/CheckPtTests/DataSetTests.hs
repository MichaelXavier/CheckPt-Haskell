module CheckPtTests.DataSetTests (dataSetTests) where

import Data.List (intercalate)

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

import qualified CheckPt.DataSet as DS
import qualified CheckPt.MediaItem as MI
import qualified CheckPt.MediaCollection as MC

dataSetTests = [group1, group2, group3, group4]

-- Fixtures (yay functional programming!)
mc1 = MC.MediaCollection { MC.name = "Foos", MC.items = []}
mc2 = MC.MediaCollection { MC.name = "Bars", MC.items = []}
mi1 = MI.MediaItem { MI.name = "Foo1", MI.completed = False}
mi2 = MI.MediaItem { MI.name = "Foo2", MI.completed = False}

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

test5 = testCase "Appends \".checkpt.json\" to the path" $ DS.dataSetPath base @?= final
          where base = "/home/michael"
                final = "/home/michael/.checkpt.json"

group3 = testGroup "DataSet stringify" [test6, test7, test8]

test6 = testCase "Correctly serializes an empty dataset" $ DS.stringify ds @?= json
          where json = "{\"collections\":[],\"items\":[]}"
                ds   = DS.DataSet { DS.collections = [], DS.items = [] }

test7 = testCase "Serializes Collections" $ DS.stringify ds @?= json
          where ds   = DS.DataSet { DS.collections = [mc1], DS.items = [] }
                json = "{\"collections\":[{\"name\":\"Foos\",\"items\":[]}],\"items\":[]}"

test8 = testCase "Serializes Items" $ DS.stringify ds @?= json
          where ds   = DS.DataSet { DS.collections = [], DS.items = [mi1] }
                json = "{\"collections\":[],\"items\":[{\"name\":\"Foo1\",\"completed\":false}]}"


group4 = testGroup "DataSet show" [test9, test10, test11, test12]

test9 = testCase "Displays an empy string when empty" $ show ds @?= str
          where str = ""
                ds  = DS.DataSet { DS.collections = [], DS.items = [] }

test10 = testCase "Displays only collections when items empty" $ show ds @?= str
          where str = show mc1 ++ "\n" ++ show mc2
                ds  = DS.DataSet { DS.collections = [mc1, mc2], DS.items = [] }

test11 = testCase "Displays only items when collections empty" $ show ds @?= str
          where str = show mi1 ++ "\n" ++ show mi2
                ds  = DS.DataSet { DS.collections = [], DS.items = [mi1, mi2] }

test12 = testCase "Displays items first" $ show ds @?= str
          where str = show mi1 ++ "\n" ++  show mi2 ++ "\n" ++ show mc1 ++ "\n" ++ show mc2
                ds  = DS.DataSet { DS.collections = [mc1, mc2], DS.items = [mi1, mi2] }
