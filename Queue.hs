

module Queue
(
  Queue,
  push,
  pop,
  peek,
  pushQueue,
  emptyQueue,
  size
) where

data Queue a = Queue { inbox :: [a], outbox :: [a], size :: Int }

emptyQueue :: Queue a
emptyQueue = Queue { inbox = [] :: [a], outbox = [] :: [a], size = 0 }

push :: Queue a -> a -> Queue a
push (Queue inb outb size) x = Queue (x:inb) outb (size + 1)

-- Push all elements of Queue1 onto Queue2
pushQueue :: Queue a -> Queue a -> Queue a
pushQueue q1 q2 =
  case poppedElement of
    | Nothing -> q2
    | Just x  -> pushQueue q' (push q2 x)
  where
    (poppedElement, q') = pop q

pop :: Queue a -> (Maybe a, Queue a)
pop q =
  case top of
    | Nothing -> (top, emptyQueue)
    | Just e  -> (Just e, poppedQueue)
  where
    (top, q') = peek q
    poppedQueue = Queue (inbox q') (tail $ outbox q') (size q' - 1)

peek :: Queue a -> (Maybe a, Queue a)
peek q@(Queue [] [] s)   = (Nothing, q)
peek q@(Queue inb [] s)  = peek $ Queue [] (reverse inb) s
peek q@(Queue [] outb s) = (Just $ head outb, q)
