module ISX.Pick.Spellchecker.Zone.Common.Data (
    create
    ) where


import              Data.Aeson
import              ISX.Pick.Spellchecker.Checker
import              ISX.Pick.Spellchecker.Parser
import              PVK.Com.API.Resource.ISXPickSnap        ()
import              Snap.Core
import              Snap.Extras.JSON
import qualified    Data.Set                                as  S
import qualified    ISX.Pick.Spellchecker.Resource.Common   as  R
import qualified    PVK.Com.API.Req                         as  Req
import qualified    PVK.Com.API.Res                         as  Res
import qualified    PVK.Com.API.Resource.ISXPick            as  R


create :: Snap ()
create = do
    req_      <- Req.getBoundedJSON' s >>= Req.validateJSON
    Just rock <- Res.runValidate req_
    let texts = parse rock
    let dicts = maybe [] R.rockMetaConfigDicts (reparseConfig rock)
    results <- liftIO $ check dicts texts
    let results' = filter isMistake results
    writeJSON $ R.Ore {
        R.oreData = toJSON results',
        R.oreUrls = S.empty}
    where
        s = 50000000 -- 50 MB
        isMistake = not . null . paraResultResults


reparseConfig :: R.Rock -> Maybe R.RockMetaConfig
reparseConfig = decode . encode . R.rockMetaConfig . R.rockMeta
