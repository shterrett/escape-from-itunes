module Channel where

-- This code liberally borrowed from _Parallel and Concurrent Programming
-- in Haskell_ by Simon Marlow: TBQueue.hs p 190

import Control.Concurrent.STM ( STM
                              , TVar
                              , newTVar
                              , readTVar
                              , writeTVar
                              , retry)

data Channel a = Channel (TVar Int) (TVar [a]) (TVar [a])

newChannel :: Int -> STM (Channel a)
newChannel size = Channel <$> (newTVar size) <*> (newTVar []) <*> (newTVar [])

writeChan :: [a] -> Channel a -> STM ()
writeChan as (Channel size _read write) = do
    let count = length as
    avail <- readTVar size
    if avail < count
      then retry
      else writeTVar size (avail - count)
    listend <- readTVar write
    writeTVar write (as ++ listend)

readChan :: Channel a -> STM a
readChan (Channel size read write) = do
    avail <- readTVar size
    writeTVar size (avail + 1)
    xs <- readTVar read
    case xs of
      (x:xs') -> do writeTVar read xs'
                    return x
      [] -> do ys <- readTVar write
               case ys of
                 [] -> retry
                 _ -> do let (z:zs) = reverse ys
                         writeTVar write []
                         writeTVar read zs
                         return z
