{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}


module Isoxya.Plugin.Spellchecker.Type (
    Spellchecker(..),
    ) where


import Control.Lens (makeLenses)


data Spellchecker = Spellchecker {}

makeLenses ''Spellchecker
