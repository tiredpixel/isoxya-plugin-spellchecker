module ISX.Plug.Spellchecker.Init (
    initSpellchecker,
    routesSpellchecker,
    ) where


import           ISX.Plug.Spellchecker.Types
import           Snap.Core
import           Snap.Snaplet
import           TPX.Com.Snap.CoreUtils
import qualified ISX.Plug.Spellchecker.Zone.Apex as ZA
import qualified ISX.Plug.Spellchecker.Zone.Data as ZD


initSpellchecker :: SnapletInit b Spellchecker
initSpellchecker = makeSnaplet "Spellchecker" "" Nothing $ do
    addRoutes routesSpellchecker
    return Spellchecker

routesSpellchecker :: [(ByteString, Handler b Spellchecker ())]
routesSpellchecker = [
    ("",                                    ifTop           ZA.apex),
    --
    ("data",                                method POST     ZD.create),
    ("data/:_",                                             notFound),
    --
    ("",                                                    notFound)]
