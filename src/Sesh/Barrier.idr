module Sesh.Barrier

import Data.IORef
import System.Concurrency.Raw

export
record Barrier where
    constructor MkBarrier
    lock       : Mutex
    cond       : Condition
    countRef   : IORef Int
    numThreads : Int

export
newBarrier : Int -> IO Barrier
newBarrier numThreads = do
    lock <- makeMutex
    cond <- makeCondition
    countRef <- newIORef 0
    pure (MkBarrier lock cond countRef numThreads)

export
waitBarrier : Barrier -> IO Bool
waitBarrier (MkBarrier lock cond countRef numThreads) = do
    mutexAcquire lock
    count <- readIORef countRef
    let count = count + 1
    writeIORef countRef count
    if count < numThreads
       then do conditionWait cond lock
               pure False
       else do conditionBroadcast cond
               pure True
