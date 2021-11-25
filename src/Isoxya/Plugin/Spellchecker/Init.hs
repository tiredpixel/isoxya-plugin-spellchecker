module Isoxya.Plugin.Spellchecker.Init (
    initSpellchecker,
    routesSpellchecker,
    ) where


import           Isoxya.Plugin.Spellchecker.Types
import           Snap.Core
import           Snap.Snaplet
import           TiredPixel.Common.Snap.CoreUtils
import qualified Isoxya.Plugin.Spellchecker.Endpoint.Apex as EA
import qualified Isoxya.Plugin.Spellchecker.Endpoint.Data as ED


initSpellchecker :: SnapletInit b Spellchecker
initSpellchecker = makeSnaplet "Spellchecker" "" Nothing $ do
    addRoutes routesSpellchecker
    return Spellchecker

routesSpellchecker :: [(ByteString, Handler b Spellchecker ())]
routesSpellchecker = [
    ("",        ifTop       EA.apex),
    --
    ("data",    method POST ED.create),
    ("data/:_",             notFound),
    --
    ("",                    notFound)]
