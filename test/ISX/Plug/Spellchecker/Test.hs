module ISX.Plug.Spellchecker.Test (
    module ISX.Plug.Spellchecker,
    module ISX.Plug.Spellchecker.Core,
    module TPX.Com.Snap.Test,
    fixturePage,
    fixtureResult,
    fixtureText,
    genPlugProcI,
    snapSpellchecker,
    ) where


import           ISX.Plug.Spellchecker
import           ISX.Plug.Spellchecker.Core hiding (addHeader, setContentType, setHeader, (.=))
import           Network.URI
import           TPX.Com.Isoxya.PlugProc
import           TPX.Com.Snap.Test          hiding (Result)
import           TPX.Com.URI
import qualified Data.Map                   as M
import qualified Data.Text                  as T


fixturePage :: Text -> FilePath
fixturePage url = toString $ "test/fixture/pages/" <> fxExt url

fixtureResult :: Text -> Text -> FilePath
fixtureResult ns url = toString $ "test/fixture/results/" <> ns <> "/" <>
    fxExt url <> ".json"

fixtureText :: Text -> FilePath
fixtureText url = toString $ "test/fixture/texts/" <> fxExt url <> ".txt"

genPlugProcI :: MonadIO m => Text -> Maybe Value -> m PlugProcI
genPlugProcI url config = do
    body <- liftIO $ readFileBS $ fixturePage url
    return PlugProcI {
        plugProcIMeta   = meta,
        plugProcIHeader = M.empty,
        plugProcIBody   = body}
    where
        Just metaURL = URIAbsolute <$>
            parseAbsoluteURI (toString $ "http://" <> url)
        meta = PlugProcIMeta {
            plugProcIMetaURL      = metaURL,
            plugProcIMetaMethod   = "GET",
            plugProcIMetaStatus   = Just 200,
            plugProcIMetaDuration = Nothing,
            plugProcIMetaErr      = Nothing,
            plugProcIMetaConfig   = config}

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
