module ISX.Test (
    module ISX.Factory,
    module PVK.Com.API.Test,
    assertTextsLookup,
    withSrv
    ) where


import              ISX.Factory
import              ISX.Pick.Spellchecker.Route
import              PVK.Com.API.Test
import qualified    Data.Text                               as  T


assertTextsLookup :: [Text] -> Text -> IO ()
assertTextsLookup texts url = do
    texts0 <- readFileText $ fixtureText url
    unlines texts `shouldBe` texts0

withSrv :: RequestBuilder IO () -> IO Response
withSrv r = runHandler r site


fixtureText :: Text -> FilePath
fixtureText url = toString $ "test/fixture/texts/" <> f <> ".txt"
    where
        f = if T.takeEnd 1 url == "/"
            then url <> "index.html"
            else url
