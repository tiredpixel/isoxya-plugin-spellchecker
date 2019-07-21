module ISX.Pick.Spellchecker.Route (site) where


import              Snap.Core
import qualified    ISX.Pick.Spellchecker.Zone.Common.Apex  as  ZA
import qualified    PVK.Com.API.Zone.Common.Error           as  ZE


site :: Snap ()
site = ifTop ZA.apex <|> route [
    -- COMMON
    --
    ("",                                                notFound)]
    where
        notFound = ZE.notFound
