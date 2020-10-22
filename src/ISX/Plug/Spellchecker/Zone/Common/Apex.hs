module ISX.Plug.Spellchecker.Zone.Common.Apex (
    apex
    ) where


import              Data.Version                            (showVersion)
import              Paths_isx_plug_spellchecker             (version)
import              Snap.Core
import              Snap.Extras.JSON
import qualified    Data.Time.Clock                         as  Clock
import qualified    ISX.Plug.Spellchecker.Resource.Common   as  R


apex :: Snap ()
apex = do
    t <- liftIO Clock.getCurrentTime
    let v = toText $ showVersion version
    writeJSON $ R.Apex t v
