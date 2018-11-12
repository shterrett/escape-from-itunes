module Args where

import Options.Applicative
import Data.Semigroup ((<>))
import Text.Regex (mkRegex, splitRegex)
import Attributes
import Actions (ActionType (DryRun, CopyFiles))

data Args = Args {
          source :: String
          , target :: String
          , attributes :: [Attribute]
          , actionType :: ActionType
          }
  deriving (Show, Eq)

getArgs :: Parser Args
getArgs = Args
  <$> strOption
    ( long "source" <>
      short 's' <>
      help "Root of existing library")
  <*> strOption
    ( long "target" <>
      short 't' <>
      help "Root of new library")
  <*> attrList
  <*> flag CopyFiles DryRun
    ( long "dry-run" <>
      short 'd' <>
      help "Print intended work, but do not copy any files")

attrList :: Parser [Attribute]
attrList = option (maybeReader attrList) (
    long "attributes" <>
    short 'a' <>
    help "comma-separated list of orderedf attributes for building directory tree")
  where attrList = sequence . (fmap toAttribute) . (splitRegex (mkRegex ","))
        helpString = "comma-separated list of ordered attributes for building directory tree. Chosen from artist, title, album, year, track, composer"

app :: ParserInfo Args
app = info (getArgs <**> helper)
      ( fullDesc
     <> progDesc "Copies an iTunes music library to a new directory tree built from the ID3 tag hierarchy provided"
     <> header "Make your music library make sense" )
