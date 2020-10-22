module ISX.Test (
    module ISX.Factory,
    module TPX.Com.API.Test,
    assertResultsLookup,
    assertTextsLookup,
    withSrv
    ) where


import              Data.Vector                             (Vector)
import              ISX.Factory
import              ISX.Plug.Spellchecker.Route
import              TPX.Com.API.Test


assertResultsLookup :: Vector Value -> Text -> Text -> IO ()
assertResultsLookup results ns url = do
    results0 <- readFileText $ fixtureResult ns url
    let Just results0' = decode $ encodeUtf8 results0 :: Maybe (Vector Value)
    results `shouldBe` results0'

assertTextsLookup :: [Text] -> Text -> IO ()
assertTextsLookup texts url = do
    texts0 <- readFileText $ fixtureText url
    unlines texts `shouldBe` texts0

withSrv :: RequestBuilder IO () -> IO Response
withSrv r = runHandler r site


fixtureResult :: Text -> Text -> FilePath
fixtureResult ns url = toString $
    "test/fixture/results/" <> ns <> "/" <> fxExt url <> ".json"

fixtureText :: Text -> FilePath
fixtureText url = toString $ "test/fixture/texts/" <> fxExt url <> ".txt"
