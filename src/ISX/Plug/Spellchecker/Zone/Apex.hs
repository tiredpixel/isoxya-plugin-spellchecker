module ISX.Plug.Spellchecker.Zone.Apex (
    apex,
    ) where


import Data.Time.Clock
import Data.Version                (showVersion)
import ISX.Plug.Spellchecker.Core
import Paths_isx_plug_spellchecker (version)


apex :: Handler b Spellchecker ()
apex = do
    t <- liftIO getCurrentTime
    let v = toText $ showVersion version
    writeJSON $ Apex t v
