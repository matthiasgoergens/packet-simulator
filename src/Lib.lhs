> {-# LANGUAGE LambdaCase, PartialTypeSignatures, GeneralizedNewtypeDeriving #-}
> module Lib where
> import Control.Concurrent.STM
> import Control.Concurrent.STM.TVar
> import Control.Monad
> import Control.Monad.Random.Strict
> import Control.Concurrent

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

> type Network x = IO x

> transmit :: Package a -> Network Transmit
> transmit _ = undefined

> listen = undefined

> sendPayload :: Package a -> Network _
> sendPayload p = do
>   id <- transmit p
>   listen >>= \case
>       Right id' | id == id' -> undefined
>       Left _ -> undefined  -- backoff, send again.

> listenAndConfirm :: NodeId -> Network _
> listenAndConfirm me = do
>   listen >>= \case
>       Right (Package a) -> undefined
>       Right _ -> return ()
>       Left _ -> return ()

> aloha :: _ -> RandT StdGen IO () -> _ -> Rational -> [Package Int] -> RandT StdGen IO ()
> aloha wait senseUntilIdle send pSend = loop where
>   loop :: [Package Int] -> RandT StdGen IO ()
>   loop (Package p:ps) = do
>       senseUntilIdle
>       join $ fromList [ (send p >> loop ps, pSend)
>                       , (wait 1 >> loop (Package p:ps), 1-pSend)]
>   loop [] = return ()

> confirm = undefined

> senseUntilIdle :: TVar Time -> TVar CarrierSense -> RandT StdGen IO ()
> senseUntilIdle ticker channel = lift $ atomically $ do
>   readTVar channel >>= \case
>       x  | x <= 0 -> return ()
>       _ -> retry

Also wait for channel to be non-Busy?

> send ticker transmitter ack channel length = lift $ atomically $ do
>   Time t <- readTVar ticker
>   writeTVar transmitter (Time $ t+length)
>   --- Needs to report success.

> wait ticker i = lift $ do
>   n <- atomically $ readTVar ticker
>   atomically $ do
>       n' <- readTVar ticker
>       unless (n + i <= n') retry

Consider enforcing a one-tick wait here:

> setup :: Rational -> Int -> Int -> Int -> RandT _ IO _
> setup pSend numNodes numPackets length = do
>   tick <- lift $ atomically $ newTVar 0
>   channel <- lift $ atomically $ newTVar 0
>   let makeNode = do
>           s <- getSplit
>           transmitter <- lift $ atomically $ newTVar 0
>           ack <- lift $ atomically $ newTVar 0
>           node <- lift $ forkIO $ evalRandT
>               (aloha (wait tick)
>                      (senseUntilIdle tick channel)
>                      (send tick transmitter ack channel)
>                      pSend
>                      (replicate numPackets (Package length)))
>               s
>           return (transmitter, node)
>   nodes <- replicateM numNodes makeNode
>   return nodes
