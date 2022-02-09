module Isoxya.Plugin.Spellchecker.Test (
    module Isoxya.Plugin.Spellchecker,
    module Isoxya.Plugin.Spellchecker.Core,
    module TiredPixel.Common.Snap.Test,
    fixturePage,
    fixtureResult,
    fixtureText,
    genProcessorI,
    snapSpellchecker,
    ) where


import qualified Data.Map                           as M
import qualified Data.Text                          as T
import           Isoxya.Plugin.Spellchecker
import           Isoxya.Plugin.Spellchecker.Core    hiding (addHeader,
                                                     setContentType, setHeader,
                                                     (.=))
import           Network.URI
import           TiredPixel.Common.Isoxya.Processor
import           TiredPixel.Common.Snap.Test        hiding (Result)
import           TiredPixel.Common.URI


fixturePage :: Text -> FilePath
fixturePage url = toString $ "test/fixture/pages/" <> fxExt url

fixtureResult :: Text -> Text -> FilePath
fixtureResult ns url = toString $ "test/fixture/results/" <> ns <> "/" <>
    fxExt url <> ".json"

fixtureText :: Text -> FilePath
fixtureText url = toString $ "test/fixture/texts/" <> fxExt url <> ".txt"

genProcessorI :: MonadIO m => Text -> Maybe Value -> m ProcessorI
genProcessorI url config = do
    body <- liftIO $ readFileBS $ fixturePage url
    return ProcessorI {
        processorIBody   = body,
        processorIHeader = M.empty,
        processorIMeta   = meta}
    where
        Just metaURL = URIAbsolute <$>
            parseAbsoluteURI (toString $ "http://" <> url)
        meta = ProcessorIMeta {
            processorIMetaConfig   = config,
            processorIMetaDuration = Nothing,
            processorIMetaError    = Nothing,
            processorIMetaMethod   = "GET",
            processorIMetaStatus   = Just 200,
            processorIMetaURL      = metaURL}

snapSpellchecker :: SpecWith (SnapHspecState Spellchecker) -> Spec
snapSpellchecker = snap (route routesSpellchecker) initSpellcheckerTest


fxExt :: Text -> Text
fxExt url = if T.takeEnd 1 url == "/"
    then url <> "index.html"
    else url

initSpellcheckerTest :: SnapletInit b Spellchecker
initSpellcheckerTest = makeSnaplet "Spellchecker" "" Nothing $ do
    addRoutes routesSpellchecker
    return Spellchecker
