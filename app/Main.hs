module Main where

import Options.Applicative (execParser)
import Args
import Itunes

main :: IO ()
main = execParser app >>= run

run :: Args -> IO ()
run = putStrLn . show

escape :: Transform -> Action -> [Directory] ->  IO ()
escape = undefined
