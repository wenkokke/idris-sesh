module Sesh.OneShot

import Data.IORef
import Sesh.Barrier
import System.Concurrency.Raw

export
data Sender : Type -> Type where
     MkSender : IORef (Maybe a) -> Barrier -> Sender a

export
data Receiver : Type -> Type where
     MkReceiver : IORef (Maybe a) -> Barrier -> Receiver a

export
newOneShot : IO (Sender a, Receiver a)
newOneShot = do
    ref <- newIORef Nothing
    barrier <- newBarrier 2
    pure (MkSender ref barrier, MkReceiver ref barrier)

export
sendOneShot : a -> Sender a -> IO ()
sendOneShot x (MkSender ref barrier) = do
    writeIORef ref (Just x)
    waitBarrier barrier
    pure ()

partial
unsafe_fromJust : Maybe a -> a
unsafe_fromJust (Just x) = x

export
receiveOneShot : Receiver a -> IO a
receiveOneShot (MkReceiver ref barrier) = do
    waitBarrier barrier
    x <- readIORef ref
    pure (assert_total unsafe_fromJust x)
