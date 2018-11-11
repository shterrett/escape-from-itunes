module Itunes where

import Control.Monad (filterM)
import Data.List (foldl')
import System.IO (FilePath)
import System.FilePath ((</>), takeFileName)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import ID3.Simple (readTag, Tag)
import Attributes (Attribute, getAttribute)

type From = FilePath
type To = FilePath
type Directory = FilePath
type PathSegment = String

data Copy = Copy { from :: From
                 , to :: To
                 }

type Action = Copy -> IO ()
type Transform = From -> IO To

handleDirectory :: Directory -> Transform -> IO ([Directory], [Copy])
handleDirectory dir t = contents dir >>=
    (\(dirs, files) -> ((,) dirs) <$> (sequence $ mkTarget t <$> files))

contents :: Directory -> IO ([Directory], [FilePath])
contents d = do
    contents <- (fmap $ (</>) d) <$> listDirectory d
    dirs <- filterM doesDirectoryExist contents
    files <- filterM doesFileExist contents
    return (dirs, files)

mkTarget :: (From -> IO To) -> From -> IO Copy
mkTarget t f = Copy f <$> t f

mkTransform :: Directory -> [Attribute] -> (From -> IO To)
mkTransform target attrs = transform
  where transform f = defaultPath target f <$>
                      (fmap $ joinPath target f) <$>
                      (=<<) (readAttrs attrs) <$>
                      (readTag f)

readAttrs :: [Attribute] -> Tag -> Maybe [PathSegment]
readAttrs attrs tag = sequence $ getAttribute <$> attrs <*> [tag]

joinPath :: Directory -> From -> [PathSegment] -> To
joinPath target f path = (foldl' (</>) target path) </> (takeFileName f)

defaultPath :: Directory -> From -> Maybe To -> To
defaultPath target _ (Just p) = p
defaultPath target f Nothing = (target </> "<Nothing>") </> (takeFileName f)
