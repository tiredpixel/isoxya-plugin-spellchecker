module Isoxya.Plugin.Spellchecker.ParserSpec (spec) where


import           Isoxya.Plugin.Spellchecker.Test


spec :: Spec
spec = snapSpellchecker $ do
    describe "example.com" $
        it "apex" $ do
            (a, e) <- load "example.com/"
            a `shouldBe` e

    describe "www.pavouk.tech" $ do
        it "apex" $ do
            (a, e) <- load "www.pavouk.tech/"
            a `shouldBe` e

        it "robots" $ do
            (a, e) <- load "www.pavouk.tech/robots.txt"
            a `shouldBe` e

        it "image" $ do
            (a, e) <- load "www.pavouk.tech/wp-content/themes/pv-www-theme-2.1.1/assets/images/logo/pv-center.svg.inv.svg.png"
            a `shouldBe` e

    describe "www.tiredpixel.com" $
        it "apex" $ do
            (a, e) <- load "www.tiredpixel.com/"
            a `shouldBe` e


load :: MonadIO m => Text -> m (Text, Text)
load url = do
    i <- genProcessorI url Nothing
    let textsA = parse i
    textsE <- readFileText $ fixtureText url
    return (unlines textsA, textsE)
