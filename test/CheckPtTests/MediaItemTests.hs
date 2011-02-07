module CheckPtTests.MediaItemTests (mediaItemTests) where

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

import qualified CheckPt.MediaItem as MI

mediaItemTests = [group1, group2]

group1 = testGroup "MediaItem complete" [test1, test2]

test1 = testCase "Marks incomplete item as complete" $ (MI.completed $ MI.complete mi) @?= True
          where mi = MI.MediaItem { MI.name = "Foo", MI.completed = False }

test2 = testCase "Does nothing on completed item" $ (MI.completed $ MI.complete mi) @?= True
          where mi = MI.MediaItem { MI.name = "Foo", MI.completed = True }

group2 = testGroup "MediaItem uncomplete" [test3, test4]

test3 = testCase "Marks complete item as incomplete" $ (MI.completed $ MI.uncomplete mi) @?= False
          where mi = MI.MediaItem { MI.name = "Foo", MI.completed = True }

test4 = testCase "Does nothing on incomplete item" $ (MI.completed $ MI.uncomplete mi) @?= False
          where mi = MI.MediaItem { MI.name = "Foo", MI.completed = False }
