module System.Concurrency.Channel.Session

import Control.Linear.LIO
import System.Concurrency
import System.Concurrency.Channel.Linear

public export
data SessionType : Type where
    Send : Type -> SessionType -> SessionType
    Receive : Type -> SessionType -> SessionType
    End : SessionType

public export
dual : SessionType -> SessionType
dual (Send a s) = Receive a (dual s)
dual (Receive a s) = Send a (dual s)
dual End = End

export
dualInv : (s : SessionType) -> dual (dual s) = s
dualInv (Send a s) = rewrite dualInv s in Refl
dualInv (Receive a s) = rewrite dualInv s in Refl
dualInv End = Refl

export
data Session : SessionType -> Type where
    MkSend : (1 sender : Sender (a, Session (dual s))) -> Session (Send a s)
    MkReceive : (1 receiver : Receiver (a, Session s)) -> Session (Receive a s)
    MkEnd : (1 barrier : Barrier) -> Session End

export
makeSession : LinearIO io =>
              {1 s : SessionType} ->
              L io (Session s, Session (dual s))
makeSession {s = Send a s} = do
    (sender, receiver) <- makeLinearChannel
    pure (MkSend sender, MkReceive receiver)
makeSession {s = Receive a s} = do
    (sender, receiver) <- makeLinearChannel
    pure (MkReceive receiver, MkSend (rewrite dualInv s in sender))
makeSession {s = End} = do
    barrier <- makeBarrier 2
    pure (MkEnd barrier, MkEnd barrier)

export
sessionSend : LinearIO io =>
              {1 s : SessionType} ->
              (1 sess : Session (Send a s)) ->
              (1 val : a) ->
              L io (Session s)
sessionSend {s = s} (MkSend sender) val = do
    (myCont, theirCont) <- makeSession {s = s}
    linearChannelSend sender (val, theirCont)
    pure myCont

export
sessionReceive : LinearIO io =>
                 (1 sess : Session (Receive a s)) ->
                 L io (a, Session s)
sessionReceive (MkReceive receiver) = do
    linearChannelReceive receiver

export
sessionEnd : LinearIO io =>
             (1 sess : Session End) ->
             L io ()
sessionEnd (MkEnd barrier) = do
    assert_linear barrierWait barrier
