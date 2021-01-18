module ISX.Factory (
    fPlugProcI,
    fxExt,
    ) where


import              Data.Aeson
import              Network.URI
import              TPX.Com.ISX.PlugProc
import              TPX.Com.URI
import qualified    Data.Map.Strict                         as  Map
import qualified    Data.Text                               as  T


fPlugProcI :: Text -> Maybe Value -> IO PlugProcI
fPlugProcI url config = do
    body <- readFileBS $ fixturePage url
    return PlugProcI {
        plugProcIMeta   = meta,
        plugProcIHeader = Map.empty,
        plugProcIBody   = body}
    where
        Just metaURL = parseURL url
        meta = PlugProcIMeta {
            plugProcIMetaURL    = metaURL,
            plugProcIMetaStatus = Just 200,
            plugProcIMetaConfig = config}

fxExt :: Text -> Text
fxExt url = if T.takeEnd 1 url == "/"
    then url <> "index.html"
    else url


fixturePage :: Text -> FilePath
fixturePage url = toString $ "test/fixture/pages/" <> fxExt url

parseURL :: Text -> Maybe URIAbsolute
parseURL url = URIAbsolute <$> parseAbsoluteURI (toString $ "http://" <> url)
