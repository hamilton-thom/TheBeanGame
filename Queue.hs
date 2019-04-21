

module Queue
(
  Queue,
  push,
  pop,
  popN,
  peek,
  pushQueue,
  emptyQueue,
  fromList,
  toList,
  size
) where

data Queue a = Queue { inbox :: [a], outbox :: [a], size :: Int }

emptyQueue :: Queue a
emptyQueue = Queue { inbox = [] :: [a], outbox = [] :: [a], size = 0 }

push :: a -> Queue a -> Queue a
push x (Queue inb outb size) = Queue (x:inb) outb (size + 1)

-- Push all elements of the first queue onto the end of the second queue.
pushQueue :: Queue a -> Queue a -> Queue a
pushQueue q1 q2 =
  case poppedElement of
    | Nothing -> q2
    | Just x  -> pushQueue q' (push x q2)
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

popN :: Queue a -> Int -> (Maybe (Queue a), Queue a)
popN q n =
  if size q >= n then (Just unsafeQ, resultQ)
    else (Nothing, q)
  where
    (unsafeQ, resultQ) = unsafePopN n

unsafePopN :: Queue a -> Int -> (Queue a, Queue a)
unsafePopN q 0 = (emptyQueue, q)
unsafePopN q n = (push nextQ x, q'')
  where
    (x, q') = pop q
    (nextQ, q'') = unsafePopN q' (n - 1)

peek :: Queue a -> (Maybe a, Queue a)
peek q@(Queue [] [] s)   = (Nothing, q)
peek q@(Queue inb [] s)  = peek $ Queue [] (reverse inb) s
peek q@(Queue [] outb s) = (Just $ head outb, q)

fromList :: [a] -> Queue a
fromList [] = emptyQueue
fromList (x:xs) = push x (buildQueue xs)

toList :: Queue a -> [a]
toList (Queue inbx outbx _) = outbx ++ (reverse inbx)
