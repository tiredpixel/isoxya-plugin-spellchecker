module ISX.Test (
    module ISX.Factory,
    module PVK.Com.API.Test,
    assertResultsLookup,
    assertTextsLookup,
    withSrv
    ) where


import              ISX.Factory
import              ISX.Pick.Spellchecker.Route
import              PVK.Com.API.Test


assertResultsLookup :: [Value] -> Text -> IO ()
assertResultsLookup results url = do
    results0 <- readFileText $ fixtureResult url
    let Just results0' = decode $ encodeUtf8 results0 :: Maybe [Value]
    results `shouldBe` results0'

assertTextsLookup :: [Text] -> Text -> IO ()
assertTextsLookup texts url = do
    texts0 <- readFileText $ fixtureText url
    unlines texts `shouldBe` texts0

withSrv :: RequestBuilder IO () -> IO Response
withSrv r = runHandler r site


fixtureResult :: Text -> FilePath
fixtureResult url = toString $ "test/fixture/results/" <> fxExt url <> ".json"

fixtureText :: Text -> FilePath
fixtureText url = toString $ "test/fixture/texts/" <> fxExt url <> ".txt"
