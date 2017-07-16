> {-# LANGUAGE LambdaCase #-}
> module Lib where

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

So, we need to come up with a type to plug our node simulator in.

- ability to 'wait'
- assume all other computation happens instantly
- ability to 'send' and 'receive' and 'sense'
- ability to get a random number (provided by simulator)
- ability to store state

- interruption --- inversion of control.

- send messages/events to self and inject them?

> data Action a b = Send a | Receive b

> aloha :: IO ()
> aloha = do
>   handleEvents
>   aloha

> waitForEvent x = x (Send ())

> handleEvents = do
>   waitForEvent $ \case
>       Send msg -> return ()
>       Receive x -> return ()
