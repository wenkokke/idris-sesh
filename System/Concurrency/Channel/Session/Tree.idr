module System.Concurrency.Channel.Session.Tree

import Control.Linear.LIO as LIO
import System.Concurrency.Channel.Session.Raw as Raw

export
record Par (i : Type) (a : Type) where
    constructor MkPar
    runPar' : L IO a

export
data Session : Type -> SessionType -> Type where
    MkSession : Raw.Session s -> Session i s

export
runPar : (forall i . Par i a) -> a
runPar par = let MkPar prog = par {i = ()} in
             unsafePerformIO (LIO.run prog)

mutual
  export
  Functor (Par i) where
      map fn par = pure $ fn !par

  export
  Applicative (Par i) where
      pure = MkPar . pure
      (<*>) f a = pure $ !f !a

  export
  Monad (Par i) where
      MkPar p >>= k
          = MkPar $ do p' <- p
                       let MkPar kp = k p'
                       kp

||| Send a value of type `a` and return the continuation of the session `s`.
export
sessionSend : {1 s : SessionType} ->
              (1 sess : Session i (Send a s)) ->
              (1 val : a) ->
              Par i (Session i s)
sessionSend (MkSession sess) val = MkPar $ do
    myCont <- Raw.sessionSend {s = s} sess val
    pure (MkSession myCont)

||| Receive a value of type `a`, and return a pair of the received value and the
||| continuation of the session `s`.
export
sessionReceive : (1 sess : Session i (Receive a s)) ->
                 Par i (a, Session i s)
sessionReceive (MkSession sess) = MkPar $ do
    (val, myCont) <- Raw.sessionReceive sess
    pure (val, MkSession myCont)

||| End a session.
export
sessionEnd : (1 sess : Session i End) ->
             Par i ()
sessionEnd (MkSession sess) = MkPar $ do
    Raw.sessionEnd sess
