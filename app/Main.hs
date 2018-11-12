module Main where

import Control.Concurrent.Async (race_)
import GHC.Conc.Sync (atomically, STM)
import Options.Applicative (execParser)
import Args
import Itunes
import Actions (getAction)
import Channel

main :: IO ()
main = execParser app >>= run

run :: Args -> IO ()
run args = let
    action = getAction (actionType args)
    channel = newChannel 500 -- wild ass guess
    transform = mkTransform (target args) (attributes args)
  in
    (race_ (readSources channel transform [source args])
                  (doAction channel action)) >>
      putStrLn "finished"

readSources ::  STM (Channel Copy) -> Transform -> [Directory] -> IO ()
readSources _ _ [] = return ()
readSources chan t (d:ds) = do
    (dirs, copies) <- handleDirectory d t
    atomically $ enqueueCopies chan copies
    readSources chan t (ds ++ dirs)

enqueueCopies :: STM (Channel Copy) -> [Copy] -> STM ()
enqueueCopies chan cs = chan >>= writeChan cs

doAction :: STM (Channel Copy) -> Action ->  IO ()
doAction chan action =
    atomically (chan >>= readChan) >>= action
