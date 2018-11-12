module Actions where

import Itunes (Copy (Copy))
import System.Directory (copyFile)

data ActionType = DryRun
                | CopyFiles
                deriving (Show, Eq)

getAction :: ActionType -> (Copy -> IO ())
getAction DryRun = putStrLn . show
getAction CopyFiles = copy

copy :: Copy -> IO ()
copy (Copy from to) = copyFile from to
