module CheckPtTests.DataSetTests (dataSetTests) where

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

import qualified CheckPt.DataSet as DS
import qualified CheckPt.MediaItem as MI
import qualified CheckPt.MediaCollection as MC

dataSetTests = [group1, group2]

group1 = testGroup "DataSet parse" [test1, test2, test3, test4]

test1 = testCase "Parses Empty Collections" $ (DS.collections $ DS.parse json) @?= []
          where json = "{\"collections\":[],\"items\":[]}"

test2 = testCase "Parses Collections" $ (DS.collections $ DS.parse json) @?= [mc]
          where mc   = MC.MediaCollection { MC.name = "Foo", MC.items = []}
                json = "{\"collections\":[{\"name\":\"Foo\",\"items\":[]}],\"items\":[]}"

test3 = testCase "Parses Empty Items" $ (DS.items $ DS.parse json) @?= []
          where json = "{\"collections\":[],\"items\":[]}"

test4 = testCase "Parses Items" $ (DS.items $ DS.parse json) @?= [mi]
        where json = "{\"collections\":[],\"items\":[{\"name\":\"Foo\",\"completed\":false}]}"
              mi   = MI.MediaItem { MI.name = "Foo", MI.completed = False}

group2 = testGroup "DataSet datasetPath" [test5]

test5 = testCase "Appends \".checkpt.json\" to the path" $ base @?= final
          where base = "/home/michael"
                final = "/home/michael/.checkpt.json"
