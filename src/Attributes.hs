module Attributes where

import Data.Accessor
import ID3.Simple
import ID3.Type

data Attribute = Artist
  | Title
  | Album
  | Year
  | Track
  | Composer
  deriving (Show, Eq)

toAttribute :: String -> Maybe Attribute
toAttribute "artist" = Just Artist
toAttribute "title" = Just Title
toAttribute "album" = Just Album
toAttribute "year" = Just Year
toAttribute "track" = Just Track
toAttribute "composer" = Just Composer
toAttribute _ = Nothing

getAttribute :: Tag -> Attribute -> Maybe String
getAttribute tag Artist = getArtist tag
getAttribute tag Title = getTitle tag
getAttribute tag Year = getYear tag
getAttribute tag Track = getTrack tag
getAttribute tag Composer = getFrameText "TCOM" tag

getFrameText :: FrameID -> Tag -> Maybe String
getFrameText frid tag = case tag^.frame frid of
                           Nothing -> Nothing
                           Just fr -> Just (fr^.textContent)
