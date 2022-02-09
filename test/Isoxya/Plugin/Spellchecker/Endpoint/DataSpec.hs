module Isoxya.Plugin.Spellchecker.Endpoint.DataSpec (spec) where


import           Isoxya.Plugin.Spellchecker.Test
import           TiredPixel.Common.Isoxya.Processor


spec :: Spec
spec = snapSpellchecker $
    describe "create" $ do
        it "=> 200" $ do
            let req = postJSON "/data" pC
            res <- runRequest req
            test res []

        it "default => 200" $ do
            (i, dataE) <- load "default" [] "www.pavouk.tech/"
            let req = postJSON "/data" i
            res <- runRequest req
            test res dataE

        it "multi => 200" $ do
            (i, dataE) <- load "multi" ["en-gb", "cs"] "www.pavouk.tech/"
            let req = postJSON "/data" i
            res <- runRequest req
            test res dataE

        it "cs => 200" $ do
            (i, dataE) <- load "cs" ["cs"] "_test/cs/"
            let req = postJSON "/data" i
            res <- runRequest req
            test res dataE

        it "de => 200" $ do
            (i, dataE) <- load "de" ["de"] "_test/de/"
            let req = postJSON "/data" i
            res <- runRequest req
            test res dataE

        it "en => 200" $ do
            (i, dataE) <- load "en" ["en"] "_test/en/"
            let req = postJSON "/data" i
            res <- runRequest req
            test res dataE

        it "es => 200" $ do
            (i, dataE) <- load "es" ["es"] "_test/es/"
            let req = postJSON "/data" i
            res <- runRequest req
            test res dataE

        it "et => 200" $ do
            (i, dataE) <- load "et" ["et"] "_test/et/"
            let req = postJSON "/data" i
            res <- runRequest req
            test res dataE

        it "fr => 200" $ do
            (i, dataE) <- load "fr" ["fr"] "_test/fr/"
            let req = postJSON "/data" i
            res <- runRequest req
            test res dataE

        it "nl => 200" $ do
            (i, dataE) <- load "nl" ["nl"] "_test/nl/"
            let req = postJSON "/data" i
            res <- runRequest req
            test res dataE


pC :: Value
pC = object [
    ("meta", object [
        ("url", "http://example.com:80/"),
        ("method", "GET")]),
    ("header", object []),
    ("body", String "")]

load :: MonadIO m => Text -> [Text] -> Text -> m (ProcessorI, [Value])
load ns dicts url = do
    i <- genProcessorI url dicts'
    t <- readFileText $ fixtureResult ns url
    let Just dataE = decode $ encodeUtf8 t
    return (i, dataE)
    where
        dicts' = if null dicts
            then Nothing
            else Just $ object [
                ("dictionaries", toJSON dicts)]

test :: Response -> [Value] -> SnapHspecM b ()
test res dat = do
    rspStatus res `shouldBe` 200
    b <- getResponseBody res
    toList (b ^. key "data" . _Array) `shouldBeList` dat
    b ^. key "urls" . _Array `shouldMeasure` 0
    b ^. _Object `shouldMeasure` 2
