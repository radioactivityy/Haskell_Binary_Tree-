-- module Queue (
--     isEmpty,
--     addQ,
--     -- remQ
-- ) where 

-- data Queue a = Queue { front :: [a], rear :: [a] }
--     deriving (Show)

-- emptyQueue :: Queue a
-- emptyQueue = Queue [] []


-- isEmpty :: Queue a -> Bool
-- isEmpty (Queue front rear) = null front && null rear

-- addQ :: a -> Queue a -> Queue a
-- addQ n (Queue front rear)  = Queue front (rear:n)



module Queue (
    Queue,
    emptyQueue,
    enqueue,
    dequeue,
    isEmpty
) where

data Queue a = Queue { front :: [a], rear :: [a] }
    deriving (Show)

-- | Create an empty queue
emptyQueue :: Queue a
emptyQueue = Queue [] []

-- | Add an element to the back of the queue
enqueue :: a -> Queue a -> Queue a
enqueue x (Queue front rear) = Queue front (x:rear)

-- | Remove the element from the front of the queue
dequeue :: Queue a -> Maybe (a, Queue a)
dequeue (Queue [] [])     = Nothing
dequeue (Queue [] rear')  = dequeue (Queue (reverse rear') [])
dequeue (Queue (x:xs) rear) = Just (x, Queue xs rear)

-- | Check if the queue is empty
isEmpty :: Queue a -> Bool
isEmpty (Queue front rear) = null front && null rear
