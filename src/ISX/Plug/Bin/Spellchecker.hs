{-# LANGUAGE TemplateHaskell #-}


module Main (main) where


import Control.Lens                (makeLenses)
import Data.Version                (showVersion)
import ISX.Plug.Spellchecker
import Paths_isx_plug_spellchecker (version)
import Snap.Snaplet
import System.IO
import TPX.Com.Snap.CoreUtils


newtype App = App {
    _spellchecker :: Snaplet Spellchecker}

makeLenses ''App

main :: IO ()
main = do
    let ver = toText $ showVersion version
    hPutStrLn stderr $ toString ver
    serveSnaplet snapCfg initApp


initApp :: SnapletInit App App
initApp = makeSnaplet "App" "" Nothing $ do
    spellchecker' <- nestSnaplet "" spellchecker initSpellchecker
    return $ App spellchecker'
