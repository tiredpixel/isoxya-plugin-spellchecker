module Isoxya.Plugin.Spellchecker.Init (
    initSpellchecker,
    routesSpellchecker,
    ) where


import           Isoxya.Plugin.Spellchecker.Type
import           Snap.Core
import           Snap.Snaplet
import           TiredPixel.Common.Snap.CoreUtil
import qualified Isoxya.Plugin.Spellchecker.Endpoint.Apex as Apx
import qualified Isoxya.Plugin.Spellchecker.Endpoint.Data as Dat


initSpellchecker :: SnapletInit b Spellchecker
initSpellchecker = makeSnaplet "Spellchecker" "" Nothing $ do
    addRoutes routesSpellchecker
    return Spellchecker

routesSpellchecker :: [(ByteString, Handler b Spellchecker ())]
routesSpellchecker = [
    ("",        ifTop       Apx.apex),
    --
    ("data",    method POST Dat.create),
    ("data/:_",             notFound),
    --
    ("",                    notFound)]
