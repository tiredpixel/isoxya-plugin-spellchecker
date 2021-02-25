module ISX.Plug.Spellchecker.Zone.Data (
    create,
    ) where


import           ISX.Plug.Spellchecker.Core
import           TPX.Com.Isoxya.PlugProc
import           TPX.Com.Isoxya.Snap.PlugProc ()
import qualified Data.Set                     as S


create :: Handler b Spellchecker ()
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
