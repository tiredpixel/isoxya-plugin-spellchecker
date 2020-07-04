module Main (main) where


import              ISX.Plugin.Spellchecker.Route
import qualified    PVK.Com.API.Res                         as  Res
import qualified    Snap.Http.Server                        as  Srv


main :: IO ()
main = do
    cEmp <- Srv.commandLineConfig Srv.emptyConfig
    Srv.httpServe (conf cEmp) site
    where
        cLog = Srv.ConfigFileLog "-"
        conf =
            Srv.setAccessLog cLog .
            Srv.setErrorLog cLog .
            Srv.setErrorHandler Res.intErr'
