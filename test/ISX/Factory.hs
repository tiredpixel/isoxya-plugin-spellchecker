module ISX.Factory (
    fRock,
    fxExt
    ) where


import              Data.Aeson
import              Network.URI
import              TPX.Com.API.Ext.URI
import qualified    Data.Map.Strict                         as  Map
import qualified    Data.Text                               as  T
import qualified    TPX.Com.API.Resource.ISX.Pick           as  R


fRock :: Text -> Maybe Value -> IO R.Rock
fRock url config = do
    body <- readFileBS $ fixturePage url
    return R.Rock {
        R.rockMeta   = meta,
        R.rockHeader = Map.empty,
        R.rockBody   = body}
    where
        Just metaUrl = parseUrl url
        meta = R.RockMeta {
            R.rockMetaUrl        = metaUrl,
            R.rockMetaStatusCode = Just 200,
            R.rockMetaConfig     = config}

fxExt :: Text -> Text
fxExt url = if T.takeEnd 1 url == "/"
    then url <> "index.html"
    else url


fixturePage :: Text -> FilePath
fixturePage url = toString $ "test/fixture/pages/" <> fxExt url

parseUrl :: Text -> Maybe URIAbsolute
parseUrl url = URIAbsolute <$> parseAbsoluteURI (toString $ "http://" <> url)
