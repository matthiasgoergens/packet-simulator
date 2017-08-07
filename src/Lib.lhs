> {-# LANGUAGE LambdaCase, PartialTypeSignatures, GeneralizedNewtypeDeriving #-}
> module Lib where
> import Control.Concurrent.STM
> import Control.Concurrent.STM.TVar
> import Control.Monad
> import Control.Monad.Random.Strict
> import Control.Concurrent
> import qualified Control.Concurrent.STM.TVar.ReadOnly as RO
> import Control.Concurrent.STM.TVar.ReadOnly (toReadOnlyTVar, ReadOnlyTVar)


Medium Access protocols govern how and when nodes can communicate over a shared wireless
channel.

a) Discuss the differences between contention based protocols and contention-free protocols,
with the aid of examples. In particular, consider factors such as effective throughput (i.e.
actual data throughput compared to channel capacity) and control overhead.

b) Write a packet simulator that uses CSMA/CA over a 1-hop neighbourhood (i.e. all nodes can
hear each other), with p-persistent collision avoidance. Assume a data transmission rate of
1000 bps with data packets being 500 bits long. Assume each node is initialized with 10
packets to send and no new packets arrive. With the aid of your simulator:

  i. Show a plot showing effective throughput as the number of nodes (N) increases
  from 2 to 20 with p=0.5. Describe the trend you observe and give reasons for this.

  ii. For N = 10, show a plot showing effective throughput as p varies from 0.1 to 1.0 in
  steps of 0.1. Describe the trend you observe and give reasons for this.

  iii. For N varying from 2 to 20, what is the optimal p to maximize effective throughput?
  What are the implications of your findings for highly dynamic networks?


---

Don't simulate acks.

---

So, we need to come up with a type to plug our node simulator in.

- ability to 'wait'
- assume all other computation happens instantly
- ability to 'send' and 'receive' and 'sense'
- ability to get a random number (provided by simulator)
- ability to store state

- interruption --- inversion of control.

- send messages/events to self and inject them?

Abstract state machine?

a node needs: packages still to send.
waiting / not?

---

Need to get stuff done: so just stick everything in IO and use TVar.

> newtype Time = Time Int
>   deriving (Eq, Ord, Show, Num)
> newtype CarrierSense = CarrierSense Int
>   deriving (Eq, Ord, Show, Num)
> newtype PackageId = PackageId Int
>   deriving (Eq, Ord, Show)
> data Transmit = Transmit PackageId
>   deriving (Eq, Ord, Show)

> newtype NodeId = NodeId Int
>   deriving (Eq, Ord, Show)
> data Package a = Package a
>   deriving (Eq, Ord, Show)

-- model time steps explicitly?

> type Network x = RandT StdGen IO x

> aloha :: (Int -> Network ()) -> Network () -> (Int -> Network Bool) ->
>           Rational -> [Package Int] -> RandT StdGen IO ()
> aloha wait senseUntilIdle send1 pSend = loop where
>   loop :: [Package Int] -> RandT StdGen IO ()
>   loop (Package p:ps) = do
>       senseUntilIdle
>       -- No exponential back-off.
>       let send = do
>               success <- send1 p
>               if success then loop ps else loop (Package p:ps)
>       join $ fromList [ (send, pSend)
>                       , (wait 1 >> loop (Package p:ps), 1-pSend)]
>   loop [] = return ()

> confirm = undefined

> senseUntilIdle :: ReadOnlyTVar Time -> TVar CarrierSense -> RandT StdGen IO ()
> senseUntilIdle ticker channel = lift $ atomically $ do
>   readTVar channel >>= \case
>       x  | x <= 0 -> return ()
>       _ -> retry

Also wait for channel to be non-Busy?

Hack: make use of the fact that all packets are same length?

> send :: ReadOnlyTVar Time -> TVar Time -> _ -> _ -> Int -> Network Bool
> send ticker highwaterMark ack channel length = lift $ do
>   end <- atomically $ do
>       Time t <- RO.readTVar ticker
>       let end = Time $ t + length
>       writeTVar highwaterMark (Time t)
>       modifyTVar channel (1+)
>       return end
>   loopUntilJust $ atomically $ do
>     t <- RO.readTVar ticker
>     users <- readTVar channel
>     highwater <- readTVar highwaterMark
>     case () of
>      () | 1 < users -> return (Just False)
>         | end <= t -> return (Just False)
>         | highwater >= t -> retry
>         | otherwise -> do
>             writeTVar highwaterMark t
>             return Nothing

> loopUntilJust :: Monad m => m (Maybe a) -> m a
> loopUntilJust step = do
>   result <- step
>   case result of
>       Nothing -> loopUntilJust step
>       Just c -> return c

> wait :: ReadOnlyTVar Time -> TVar Time -> Int -> RandT StdGen IO ()
> wait ticker highwaterMark i = lift $ do
>   Time start <- atomically $ readTVar highwaterMark
>   atomically . writeTVar highwaterMark <=< loopUntilJust . atomically $ do
>       Time last <- readTVar highwaterMark
>       Time t <- RO.readTVar ticker
>       case () of
>        () | start + i <= t -> return (Just (Time t))
>           | last >= t -> retry
>           | otherwise -> do
>               writeTVar highwaterMark (Time t)
>               return Nothing

Consider enforcing a one-tick wait here:

> setup :: Rational -> Int -> Int -> Int -> Network _
> setup pSend numNodes numPackets length = do
>   tick <- lift $ atomically $ newTVar 0
>   channel <- lift $ atomically $ newTVar 0
>   let roTick = toReadOnlyTVar tick
>       roChannel = toReadOnlyTVar channel
>   let makeNode = do
>           s <- getSplit
>           highwaterMark <- lift $ atomically $ newTVar 0
>           ack <- lift $ atomically $ newTVar 0
>           node <- lift $ forkIO $ evalRandT
>               (aloha (wait roTick highwaterMark)
>                      (senseUntilIdle roTick channel)
>                      (send roTick highwaterMark ack channel)
>                      pSend
>                      (replicate numPackets (Package length)))
>               s
>           return (highwaterMark, node)
>   nodes <- replicateM numNodes makeNode
>   return nodes
