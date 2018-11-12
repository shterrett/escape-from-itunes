module ArgsSpec where

import Test.Hspec
import Options.Applicative
import Attributes
import Actions
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
                                [Composer, Album, Artist]
                                CopyFiles)
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
                                [Composer, Album, Artist]
                                CopyFiles)
    it "fails if the attribute list does not match allowed attributes" $ do
      getParseResult (
        execParserPure defaultPrefs app [ "--source"
                                        , "/Users/shterrett/iTunesMusic"
                                        , "--target"
                                        , "/Users/shterrett/music"
                                        , "--attributes"
                                        , "composer,period,artist"
                                        ])
        `shouldBe` Nothing
    it "sets the action to dry run when the flag is passed" $ do
      getParseResult (
        execParserPure defaultPrefs app [ "--source"
                                        , "/Users/shterrett/iTunesMusic"
                                        , "--target"
                                        , "/Users/shterrett/music"
                                        , "--attributes"
                                        , "composer,album,artist"
                                        , "--dry-run"
                                        ])
        `shouldBe` (Just $ Args "/Users/shterrett/iTunesMusic"
                                "/Users/shterrett/music"
                                [Composer, Album, Artist]
                                DryRun)
