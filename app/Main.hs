{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Concurrent.Async (concurrently)
import Control.Concurrent.BoundedChan ( BoundedChan
                                      , readChan
                                      , writeChan
                                      , newBoundedChan
                                      )
import Control.Monad (forM_)
import GHC.Conc.Sync (atomically, STM)
import Options.Applicative (execParser)
import Args
import Itunes
import Actions (getAction)
import Debug.Trace

main :: IO ()
main = execParser app >>= run

run :: Args -> IO ()
run args = let
    action = getAction (actionType args)
    channel = newBoundedChan 500 -- wild ass guess
    transform = mkTransform (target args) (attributes args)
  in 
    channel >>= (\c ->
      (concurrently (readSources transform [source args] c)
                    (doAction action c))) >>
        putStrLn "finished"

readSources :: Transform ->
               [Directory] ->
               BoundedChan (Maybe Copy) ->
               IO ()
readSources _ [] chan = writeChan chan Nothing
readSources t (d:ds) chan = do
    (dirs, copies) <- handleDirectory d t
    forM_ (Just <$> copies) (writeChan chan)
    readSources t (ds ++ dirs) chan

doAction :: Action -> BoundedChan (Maybe Copy) -> IO ()
doAction action chan = do
    copy <- readChan chan
    case copy of
      (Just c) -> action c >> doAction action chan
      Nothing -> return ()
