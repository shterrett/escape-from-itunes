module ArgsSpec where

import Test.Hspec
import Options.Applicative
import Attributes
import Args

spec = do
  describe "parsing arguments" $ do
    it "parses properly supplied short arguments" $ do
      getParseResult (
        execParserPure defaultPrefs app [ "-s"
                                        , "/Users/shterrett/iTunesMusic"
                                        , "-t"
                                        , "/Users/shterrett/music"
                                        , "-a"
                                        , "composer,album,artist"
                                        ])
        `shouldBe` (Just $ Args "/Users/shterrett/iTunesMusic"
                                "/Users/shterrett/music"
                                [Composer, Album, Artist])
    it "parses properly supplied long arguments" $ do
      getParseResult (
        execParserPure defaultPrefs app [ "--source"
                                        , "/Users/shterrett/iTunesMusic"
                                        , "--target"
                                        , "/Users/shterrett/music"
                                        , "--attributes"
                                        , "composer,album,artist"
                                        ])
        `shouldBe` (Just $ Args "/Users/shterrett/iTunesMusic"
                                "/Users/shterrett/music"
                                [Composer, Album, Artist])
