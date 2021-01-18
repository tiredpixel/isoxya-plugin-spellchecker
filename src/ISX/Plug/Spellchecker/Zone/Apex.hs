module ISX.Plug.Spellchecker.Zone.Apex (
    apex,
    ) where


import              Data.Version                            (showVersion)
import              ISX.Plug.Spellchecker.Resource
import              Paths_isx_plug_spellchecker             (version)
import              Snap.Core
import              Snap.Extras.JSON
import qualified    Data.Time.Clock                         as  Clock


apex :: Snap ()
apex = do
    t <- liftIO Clock.getCurrentTime
    let v = toText $ showVersion version
    writeJSON $ Apex t v
