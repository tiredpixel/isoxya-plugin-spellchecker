{-# LANGUAGE TemplateHaskell #-}


module Main (main) where


import           Control.Concurrent               (forkIO)
import           Control.Lens                     (makeLenses)
import           Data.Version                     (showVersion)
import           Isoxya.Plugin.Spellchecker
import           Paths_isoxya_plugin_spellchecker (version)
import           Snap.Snaplet
import           System.IO
import qualified TiredPixel.Common.Snap.Main      as S


newtype App = App {
    _spellchecker :: Snaplet Spellchecker}

makeLenses ''App

main :: IO ()
main = do
    hPutStrLn stderr $ "Isoxya plugin Spellchecker " <> toString ver
    done <- S.init
    tId <- forkIO $ serveSnaplet S.config initApp
    S.wait done tId
    where
        ver = toText $ showVersion version


initApp :: SnapletInit App App
initApp = makeSnaplet "App" "" Nothing $ do
    spellchecker' <- nestSnaplet "" spellchecker initSpellchecker
    return $ App spellchecker'
