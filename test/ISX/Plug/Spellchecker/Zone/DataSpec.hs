module ISX.Plug.Spellchecker.Zone.DataSpec (spec) where


import              ISX.Test
import              Prelude                                 hiding  (get)


spec :: Spec
spec =
    describe "create" $ do
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
            let check' = testPage "multi" ["en-gb", "cs"]
        
            describe "www.pavouk.tech" $
                it "apex" $
                    check' "www.pavouk.tech/"
        
        describe "cs" $ do
            let check' = testPage "cs" ["cs"]
        
            describe "_test" $
                it "apex" $
                    check' "_test/cs/"
        
        describe "de" $ do
            let check' = testPage "de" ["de"]
        
            describe "_test" $
                it "apex" $
                    check' "_test/de/"
        
        describe "en" $ do
            let check' = testPage "en" ["en"]
        
            describe "_test" $
                it "apex" $
                    check' "_test/en/"
        
        describe "es" $ do
            let check' = testPage "es" ["es"]
        
            describe "_test" $
                it "apex" $
                    check' "_test/es/"
        
        describe "et" $ do
            let check' = testPage "et" ["et"]
        
            describe "_test" $
                it "apex" $
                    check' "_test/et/"
        
        describe "fr" $ do
            let check' = testPage "fr" ["fr"]
        
            describe "_test" $
                it "apex" $
                    check' "_test/fr/"
        
        describe "nl" $ do
            let check' = testPage "nl" ["nl"]
        
            describe "_test" $
                it "apex" $
                    check' "_test/nl/"


pC :: Value
pC = object [
    ("meta", object [
        ("url", "http://example.com:80/")]),
    ("header", object []),
    ("body", String "")]

testPage :: Text -> [Text] -> Text -> IO ()
testPage ns dicts url = do
    ppi <- fPlugProcI url dicts'
    res <- withSrv $ postJSON "/data" ppi
    assertSuccess res
    b <- getResponseBody res
    assertResultsLookup (b ^. key "data" . _Array) ns url
    b ^.. key "urls" . values `shouldBe` []
    assertElemN res 2
    where
        dicts' = if null dicts
            then Nothing
            else Just $ object [
                ("dicts", toJSON dicts)]
