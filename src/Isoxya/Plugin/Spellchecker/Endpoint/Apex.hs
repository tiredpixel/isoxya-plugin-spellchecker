module Isoxya.Plugin.Spellchecker.Endpoint.Apex (
    apex,
    ) where


import Data.Time.Clock
import Data.Version                     (showVersion)
import Isoxya.Plugin.Spellchecker.Core
import Paths_isoxya_plugin_spellchecker (version)


apex :: Handler b Spellchecker ()
apex = do
    t <- liftIO getCurrentTime
    let v = toText $ showVersion version
    writeJSON $ Apex t v
