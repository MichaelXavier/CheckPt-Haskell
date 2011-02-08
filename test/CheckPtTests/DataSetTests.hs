module CheckPtTests.DataSetTests (dataSetTests) where

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

import qualified CheckPt.DataSet as DS
import qualified CheckPt.MediaItem as MI
import qualified CheckPt.MediaCollection as MC

dataSetTests = [group1, group2, group3]

group1 = testGroup "DataSet parseDataSet" [test1, test2, test3, test4]

test1 = testCase "Parses Empty Collections" $ (DS.collections $ DS.parseDataSet json) @?= []
          where json = "{\"collections\":[],\"items\":[]}"

test2 = testCase "Parses Collections" $ (DS.collections $ DS.parseDataSet json) @?= [mc]
          where mc   = MC.MediaCollection { MC.name = "Foo", MC.items = []}
                json = "{\"collections\":[{\"name\":\"Foo\",\"items\":[]}],\"items\":[]}"

test3 = testCase "Parses Empty Items" $ (DS.items $ DS.parseDataSet json) @?= []
          where json = "{\"collections\":[],\"items\":[]}"

test4 = testCase "Parses Items" $ (DS.items $ DS.parseDataSet json) @?= [mi]
        where json = "{\"collections\":[],\"items\":[{\"name\":\"Foo\",\"completed\":false}]}"
              mi   = MI.MediaItem { MI.name = "Foo", MI.completed = False}

group2 = testGroup "DataSet datasetPath" [test5]

test5 = testCase "Appends \".checkpt.json\" to the path" $ DS.dataSetPath base @?= final
          where base = "/home/michael"
                final = "/home/michael/.checkpt.json"

group3 = testGroup "DataSet stringify" [test6, test7, test8]

test6 = testCase "Correctly serializes an empty dataset" $ DS.stringify ds @?= json
          where json = "{\"collections\":[],\"items\":[]}"
                ds   = DS.DataSet { DS.collections = [], DS.items = [] }

test7 = testCase "Serializes Collections" $ DS.stringify ds @?= json
          where mc   = MC.MediaCollection { MC.name = "Foo", MC.items = []}
                ds   = DS.DataSet { DS.collections = [mc], DS.items = [] }
                json = "{\"collections\":[{\"name\":\"Foo\",\"items\":[]}],\"items\":[]}"


test8 = testCase "Serializes Items" $ DS.stringify ds @?= json
          where mi   = MI.MediaItem { MI.name = "Foo", MI.completed = False}
                ds   = DS.DataSet { DS.collections = [], DS.items = [mi] }
                json = "{\"collections\":[],\"items\":[{\"name\":\"Foo\",\"completed\":false}]}"

