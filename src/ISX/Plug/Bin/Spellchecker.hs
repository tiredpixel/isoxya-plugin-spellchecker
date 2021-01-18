module Main (main) where


import              Data.Version                            (showVersion)
import              ISX.Plug.Spellchecker.Route
import              Paths_isx_plug_spellchecker             (version)
import              TPX.Com.API.Res
import qualified    Snap.Http.Server                        as  Srv


main :: IO ()
main = do
    let ver = toText $ showVersion version
    putTextLn ver
    cEmp <- Srv.commandLineConfig Srv.emptyConfig
    Srv.httpServe (conf cEmp) site
    where
        cLog = Srv.ConfigFileLog "-"
        conf =
            Srv.setAccessLog cLog .
            Srv.setErrorLog cLog .
            Srv.setErrorHandler intErr'
