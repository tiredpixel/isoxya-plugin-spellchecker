module ISX.Factory (
    fProcI,
    fxExt
    ) where


import              Data.Aeson
import              Network.URI
import              TPX.Com.API.Ext.URI
import qualified    Data.Map.Strict                         as  Map
import qualified    Data.Text                               as  T
import qualified    TPX.Com.API.Resource.ISX.Proc           as  R


fProcI :: Text -> Maybe Value -> IO R.ProcI
fProcI url config = do
    body <- readFileBS $ fixturePage url
    return R.ProcI {
        R.procIMeta   = meta,
        R.procIHeader = Map.empty,
        R.procIBody   = body}
    where
        Just metaUrl = parseUrl url
        meta = R.ProcIMeta {
            R.procIMetaUrl        = metaUrl,
            R.procIMetaStatusCode = Just 200,
            R.procIMetaConfig     = config}

fxExt :: Text -> Text
fxExt url = if T.takeEnd 1 url == "/"
    then url <> "index.html"
    else url


fixturePage :: Text -> FilePath
fixturePage url = toString $ "test/fixture/pages/" <> fxExt url

parseUrl :: Text -> Maybe URIAbsolute
parseUrl url = URIAbsolute <$> parseAbsoluteURI (toString $ "http://" <> url)
