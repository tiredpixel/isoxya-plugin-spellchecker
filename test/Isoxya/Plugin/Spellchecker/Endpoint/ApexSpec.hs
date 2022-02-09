module Isoxya.Plugin.Spellchecker.Endpoint.ApexSpec (spec) where


import           Isoxya.Plugin.Spellchecker.Test


spec :: Spec
spec = snapSpellchecker $
    describe "apex" $
        it "=> 200" $ do
            let req = get "/" emptyP
            res <- runRequest req
            rspStatus res `shouldBe` 200
            b <- getResponseBody res
            b ^. key "time" . _String `shouldContain` "T"
            b ^. key "version" . _String `shouldBe` "0.0.0"
            b ^. _Object `shouldMeasure` 2
