module ISX.Plugin.Spellchecker.Zone.Common.Data (
    create
    ) where


import              Data.Aeson
import              ISX.Plugin.Spellchecker.Checker
import              ISX.Plugin.Spellchecker.Parser
import              PVK.Com.API.Resource.ISXPickSnap        ()
import              Snap.Core
import              Snap.Extras.JSON
import              System.Environment                      (lookupEnv)
import qualified    Data.Set                                as  S
import qualified    ISX.Plugin.Spellchecker.Resource.Common as  R
import qualified    PVK.Com.API.Req                         as  Req
import qualified    PVK.Com.API.Res                         as  Res
import qualified    PVK.Com.API.Resource.ISXPick            as  R


create :: Snap ()
create = do
    reqLim_ <- liftIO $ join <$> (fmap . fmap) readMaybe (lookupEnv "REQ_LIM")
    let reqLim = fromMaybe reqLimDef reqLim_
    req_      <- Req.getBoundedJSON' reqLim >>= Req.validateJSON
    Just rock <- Res.runValidate req_
    parasLim_ <- liftIO $ join <$> (fmap . fmap) readMaybe (
        lookupEnv "PARAS_LIM")
    let parasLim = fromMaybe parasLimDef parasLim_
    let texts = take parasLim $ parse rock
    let dicts = maybe [] R.rockMetaConfigDicts (reparseConfig rock)
    results <- liftIO $ check dicts texts
    let results' = filter isMistake results
    writeJSON  R.Ore {
        R.oreData = toJSON results',
        R.oreUrls = S.empty}
    where
        isMistake = not . null . paraResultResults


parasLimDef :: Int
parasLimDef = 100 -- paragraphs

reqLimDef :: Int64
reqLimDef = 2097152 -- 2 MB = (1 + .5) * (4/3) MB

reparseConfig :: R.Rock -> Maybe R.RockMetaConfig
reparseConfig = decode . encode . R.rockMetaConfig . R.rockMeta
