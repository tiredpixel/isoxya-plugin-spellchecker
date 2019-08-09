module ISX.Pick.Spellchecker.Zone.Common.DataSpec (spec) where


import              ISX.Pick.Spellchecker.Checker
import              ISX.Test
import              Prelude                                 hiding  (get)
import qualified    ISX.Pick.Spellchecker.Resource.Common   as  R


{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}


spec :: Spec
spec =
    describe "/data POST" $ do
        it "ok" $ do
            res <- withSrv $ postJSON "/data" pC
            assertSuccess res
            b <- getResponseBody res
            b ^.. key "data" . values `shouldBe` []
            b ^.. key "urls" . values `shouldBe` []
            assertElemN res 2
        
        describe "default" $ do
            let check' = testPage "default" []
        
            describe "www.pavouk.tech" $
                it "apex" $
                    check' "www.pavouk.tech/"
        
        describe "multi" $ do
            let check' = testPage "multi" [DictEnGB, DictCs]
        
            describe "www.pavouk.tech" $
                it "apex" $
                    check' "www.pavouk.tech/"


pC :: Value
pC = object [
    ("meta", object [
        ("url", "http://example.com:80/")]),
    ("header", object []),
    ("body", String "")]

testPage :: Text -> [Dict] -> Text -> IO ()
testPage ns dicts url = do
    rock <- fRock url dicts'
    res <- withSrv $ postJSON "/data" rock
    assertSuccess res
    b <- getResponseBody res
    assertResultsLookup (b ^.. key "data") ns url
    b ^.. key "urls" . values `shouldBe` []
    assertElemN res 2
    where
        dicts' = if null dicts
            then Nothing
            else Just $ toJSON $ R.RockMetaConfig dicts
