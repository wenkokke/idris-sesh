module System.Concurrency.Channel.Linear

import Control.Linear.LIO
import System.Concurrency

||| Represents the endpoint of a one-shot channel over which the value is sent.
|||
||| This is created by `makeLinearChannel`.
export
record Sender a where
    constructor MkSender
    1 chan : Channel a

||| Represents the endpoint of a one-shot channel over which the value is
||| received.
|||
||| This is created by `makeLinearChannel`.
export
record Receiver a where
    constructor MkReceiver
    1 chan : Channel a

||| Creates a one-shot channel, consisting of a `Sender` and a `Receiver`.
export
makeLinearChannel : (LinearIO io) => L io (Sender a, Receiver a)
makeLinearChannel = do
    chan <- makeChannel
    pure (MkSender chan, MkReceiver chan)

unsafe_channelGet : HasIO io => (1 chan : Channel a) -> io a
unsafe_channelGet chan = assert_linear channelGet chan

||| Receives a value over a one-shot channel, thereby consuming the `Receiver`
||| endpoint of the channel.
export
linearChannelReceive : (LinearIO io) => (1 receiver : Receiver a) -> L io a
linearChannelReceive (MkReceiver chan) = unsafe_channelGet chan

unsafe_channelPut : HasIO io => (1 chan : Channel a) -> (1 val : a) -> io ()
unsafe_channelPut chan val = assert_linear (assert_linear channelPut chan) val

||| Sends a value over a one-shot channel, thereby consuming the `Sender`
||| endpoint of the channel.
export
linearChannelSend : (LinearIO io) => (1 sender : Sender a) -> (1 val : a) -> L io ()
linearChannelSend (MkSender chan) val = unsafe_channelPut chan val
