module ISX.Plug.Spellchecker.Zone.Common.Data (
    create
    ) where


import              Data.Aeson
import              ISX.Plug.Spellchecker.Checker
import              ISX.Plug.Spellchecker.Parser
import              Snap.Core
import              Snap.Extras.JSON
import              System.Environment                      (lookupEnv)
import              TPX.Com.API.Resource.ISX.ProcSnap       ()
import qualified    Data.Set                                as  S
import qualified    ISX.Plug.Spellchecker.Resource.Common   as  R
import qualified    TPX.Com.API.Req                         as  Req
import qualified    TPX.Com.API.Res                         as  Res
import qualified    TPX.Com.API.Resource.ISX.Proc           as  R


create :: Snap ()
create = do
    reqLim_ <- liftIO $ join <$> (fmap . fmap) readMaybe (lookupEnv "REQ_LIM")
    let reqLim = fromMaybe reqLimDef reqLim_
    req_      <- Req.getBoundedJSON' reqLim >>= Req.validateJSON
    Just procI <- Res.runValidate req_
    parasLim_ <- liftIO $ join <$> (fmap . fmap) readMaybe (
        lookupEnv "PARAS_LIM")
    let parasLim = fromMaybe parasLimDef parasLim_
    let texts = take parasLim $ parse procI
    let dicts = maybe [] R.procIMetaConfigDicts (reparseConfig procI)
    results <- liftIO $ check dicts texts
    let results' = filter isMistake results
    writeJSON  R.ProcO {
        R.procOData = toJSON results',
        R.procOUrls = S.empty}
    where
        isMistake = not . null . paraResultResults


parasLimDef :: Int
parasLimDef = 100 -- paragraphs

reqLimDef :: Int64
reqLimDef = 2097152 -- 2 MB = (1 + .5) * (4/3) MB

reparseConfig :: R.ProcI -> Maybe R.ProcIMetaConfig
reparseConfig = decode . encode . R.procIMetaConfig . R.procIMeta
