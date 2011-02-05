import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit
--import Test.Framework.Providers.QuickCheck (testProperty)

import CheckPtTests.DataSetTests

main = defaultMain tests

-- concat on more tests from other modules here
tests = dataSetTests
