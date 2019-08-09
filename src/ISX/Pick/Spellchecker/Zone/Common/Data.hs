module ISX.Pick.Spellchecker.Zone.Common.Data (
    create
    ) where


import              Data.Aeson
import              ISX.Pick.Spellchecker.Checker
import              ISX.Pick.Spellchecker.Parser
import              ISX.Pick.Spellchecker.Resource.Common   ()
import              PVK.Com.API.Resource.ISXPickSnap        ()
import              Snap.Core
import              Snap.Extras.JSON
import qualified    Data.Set                                as  S
import qualified    PVK.Com.API.Req                         as  Req
import qualified    PVK.Com.API.Res                         as  Res
import qualified    PVK.Com.API.Resource.ISXPick            as  R


create :: Snap ()
create = do
    req_      <- Req.getJSON' >>= Req.validateJSON
    Just rock <- Res.runValidate req_
    let texts = parse rock
    let dicts = [] -- TODO: dicts/configs
    results <- liftIO $ check dicts texts
    let results' = filter isMistake results
    writeJSON $ R.Ore {
        R.oreData = toJSON results',
        R.oreUrls = S.empty}
    where
        isMistake = not . null . paraResultResults
