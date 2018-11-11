module ItunesSpec where

import Test.Hspec
import Attributes
import Itunes

spec :: Spec
spec =
    describe "transforming file path" $ do
      it "joins the path segments from the attributes" $ do
        joinPath "./target" "./src/artist/piece.mp3" ["composer", "album"] `shouldBe`
          "./target/composer/album/piece.mp3"

      it "anchors at target and interpolates the attribute values" $ do
        let file = "./examples/holst/mars.mp3"
        let target = "/home/stuart/music"
        let attrs = [Composer, Album]
        let transform = mkTransform target attrs
        dest <- transform file
        dest `shouldBe` "/home/stuart/music/Holst/Planets -- Atlanta Symphony/mars.mp3"
      it "uses <Nothing> as a placeholder when attrs weren't satisfied" $ do
        let file = "./examples/strauss/dinner.mp3"
        let target = "/home/stuart/music"
        let attrs = [Composer, Album]
        let transform = mkTransform target attrs
        dest <- transform file
        dest `shouldBe` "/home/stuart/music/<Nothing>/dinner.mp3"
