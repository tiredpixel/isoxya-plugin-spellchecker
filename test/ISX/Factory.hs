module ISX.Factory (
    fPlugProcI,
    fxExt
    ) where


import              Data.Aeson
import              Network.URI
import              TPX.Com.URI
import qualified    Data.Map.Strict                         as  Map
import qualified    Data.Text                               as  T
import qualified    TPX.Com.ISX.PlugProc                    as  R


fPlugProcI :: Text -> Maybe Value -> IO R.PlugProcI
fPlugProcI url config = do
    body <- readFileBS $ fixturePage url
    return R.PlugProcI {
        R.plugProcIMeta   = meta,
        R.plugProcIHeader = Map.empty,
        R.plugProcIBody   = body}
    where
        Just metaURL = parseURL url
        meta = R.PlugProcIMeta {
            R.plugProcIMetaURL    = metaURL,
            R.plugProcIMetaStatus = Just 200,
            R.plugProcIMetaConfig = config}

fxExt :: Text -> Text
fxExt url = if T.takeEnd 1 url == "/"
    then url <> "index.html"
    else url


fixturePage :: Text -> FilePath
fixturePage url = toString $ "test/fixture/pages/" <> fxExt url

parseURL :: Text -> Maybe URIAbsolute
parseURL url = URIAbsolute <$> parseAbsoluteURI (toString $ "http://" <> url)
