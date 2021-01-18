module ISX.Plug.Spellchecker.Zone.Data (
    create,
    ) where


import              Data.Aeson
import              ISX.Plug.Spellchecker.Checker
import              ISX.Plug.Spellchecker.Parser
import              ISX.Plug.Spellchecker.Resource
import              Snap.Core
import              Snap.Extras.JSON
import              TPX.Com.API.Req
import              TPX.Com.API.Res
import              TPX.Com.ISX.PlugProc
import              TPX.Com.ISX.PlugProcSnap                ()
import qualified    Data.Set                                as  S


create :: Snap ()
create = do
    req_     <- getBoundedJSON' reqLim >>= validateJSON
    Just req <- runValidate req_
    let dicts = maybe [] configDicts (reparseConfig req)
    let texts = take parasLim $ parse req
    results <- liftIO $ check dicts texts
    let results' = filter isMistake results
    writeJSON PlugProcO {
        plugProcOData = toJSON results',
        plugProcOURLs = S.empty}
    where
        isMistake = not . null . paraResultResults
        parasLim = 100 -- paragraphs
        reqLim = 2097152 -- 2 MB = (1 + .5) * (4/3) MB


reparseConfig :: PlugProcI -> Maybe Config
reparseConfig = decode . encode . plugProcIMetaConfig . plugProcIMeta
