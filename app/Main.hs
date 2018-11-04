module Main where

import Args
import Options.Applicative (execParser)

main :: IO ()
main = execParser app >>= run

run :: Args -> IO ()
run = putStrLn . show
