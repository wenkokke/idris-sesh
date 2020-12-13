module Main

import Test.Golden

--------------------------------------------------------------------------------
-- Test cases

barrierTests : TestPool
barrierTests = MkTestPool []
    [ "basic001"
    ]

semaphoreTests : TestPool
semaphoreTests = MkTestPool []
    [ "basic001"
    , "basic002"
    ]


main : IO ()
main = runner
    [ testPaths "barrier" barrierTests
    , testPaths "semaphore" semaphoreTests
    ]
    where
        testPaths : String -> TestPool -> TestPool
        testPaths dir = record { testCases $= map ((dir ++ "/") ++) }
