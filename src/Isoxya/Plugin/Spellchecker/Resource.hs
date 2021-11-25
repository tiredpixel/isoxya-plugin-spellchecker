{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module Isoxya.Plugin.Spellchecker.Resource (
    Apex(..),
    Config(..),
    ) where


import Data.Aeson                         hiding (Result)
import Data.Time.Clock
import Isoxya.Plugin.Spellchecker.Checker


data Apex = Apex {
    apexNow     :: UTCTime,
    apexVersion :: Text
    } deriving (Show)
instance ToJSON Apex where
    toJSON Apex{..} = object [
        "now"     .= apexNow,
        "version" .= apexVersion]

newtype Config = Config {
    configDictionaries :: [Dictionary]
    } deriving (Show)
instance FromJSON Config where
    parseJSON = withObject "processor_i.meta.config" $ \j -> do
        configDictionaries <- j .: "dictionaries"
        return Config{..}
instance ToJSON Config where
    toJSON Config{..} = object [
        "dictionaries" .= configDictionaries]


instance FromJSON Dictionary where
    parseJSON = withText "dictionary" $ \case
        "cs"    -> pure DictionaryCs
        "cs-cz" -> pure DictionaryCsCz
        "de"    -> pure DictionaryDe
        "de-de" -> pure DictionaryDeDe
        "en"    -> pure DictionaryEn
        "en-gb" -> pure DictionaryEnGb
        "en-us" -> pure DictionaryEnUs
        "es"    -> pure DictionaryEs
        "es-es" -> pure DictionaryEsEs
        "et"    -> pure DictionaryEt
        "et-ee" -> pure DictionaryEtEe
        "fr"    -> pure DictionaryFr
        "fr-fr" -> pure DictionaryFrFr
        "nl"    -> pure DictionaryNl
        "nl-nl" -> pure DictionaryNlNl
        _       -> fail "invalid Dictionary"
instance ToJSON Dictionary where
    toJSON = String . \case
        DictionaryCs   -> "cs"
        DictionaryCsCz -> "cs-cz"
        DictionaryDe   -> "de"
        DictionaryDeDe -> "de-de"
        DictionaryEn   -> "en"
        DictionaryEnGb -> "en-gb"
        DictionaryEnUs -> "en-us"
        DictionaryEs   -> "es"
        DictionaryEsEs -> "es-es"
        DictionaryEt   -> "et"
        DictionaryEtEe -> "et-ee"
        DictionaryFr   -> "fr"
        DictionaryFrFr -> "fr-fr"
        DictionaryNl   -> "nl"
        DictionaryNlNl -> "nl-nl"

instance ToJSON ParagraphResult where
    toJSON ParagraphResult{..} = object [
        "paragraph" .= paragraphResultParagraph,
        "results"   .= paragraphResultResults]

instance ToJSON Result where
    toJSON = \case
        ResultOk -> object [
            "correct" .= True,
            "status"  .= String "ok"]
        ResultRoot root -> object [
            "correct" .= True,
            "root"    .= root,
            "status"  .= String "root"]
        ResultCompound -> object [
            "correct" .= True,
            "status"  .= String "compound"]
        ResultMiss orig offset misses -> object [
            "correct"     .= False,
            "offset"      .= offset,
            "status"      .= String "miss",
            "suggestions" .= misses,
            "word"        .= orig]
        ResultNone orig offset -> object [
            "correct" .= False,
            "offset"  .= offset,
            "status"  .= String "none",
            "word"    .= orig]
        _ -> Null
