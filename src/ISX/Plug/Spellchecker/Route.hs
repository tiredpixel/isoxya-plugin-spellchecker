module ISX.Plug.Spellchecker.Route (site) where


import              Snap.Core
import qualified    ISX.Plug.Spellchecker.Zone.Apex         as  ZA
import qualified    ISX.Plug.Spellchecker.Zone.Data         as  ZD
import              TPX.Com.API.Res


site :: Snap ()
site = ifTop ZA.apex <|> route [
    ("data",                                method POST     ZD.create),
    ("data/:_",                                             notFound),
    --
    ("",                                                    notFound)]
