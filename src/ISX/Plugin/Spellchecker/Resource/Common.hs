{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module ISX.Plugin.Spellchecker.Resource.Common (
    Apex(..),
    RockMetaConfig(..)
    ) where


import              Data.Aeson                              hiding (Result)
import              Data.Time.Clock                         (UTCTime)
import              ISX.Plugin.Spellchecker.Checker


data Apex = Apex {
    apexTNow    :: UTCTime,
    apexVersion :: Text
    } deriving (Show)
instance ToJSON Apex where
    toJSON o = object [
        "t_now"   .= apexTNow o,
        "version" .= apexVersion o]

newtype RockMetaConfig = RockMetaConfig {
    rockMetaConfigDicts :: [Dict]
    } deriving (Show)
instance FromJSON RockMetaConfig where
    parseJSON = withObject "RockMetaConfig" $ \j -> do
        fDicts <- j .: "dicts"
        return $ RockMetaConfig fDicts
instance ToJSON RockMetaConfig where
    toJSON o = object [
        "dicts" .= rockMetaConfigDicts o]


instance FromJSON Dict where
    parseJSON = withText "Dict" $ \case
        "cs"    -> pure DictCs
        "cs-cz" -> pure DictCsCZ
        "de"    -> pure DictDe
        "de-de" -> pure DictDeDE
        "en"    -> pure DictEn
        "en-gb" -> pure DictEnGB
        "en-us" -> pure DictEnUS
        "es"    -> pure DictEs
        "es-es" -> pure DictEsES
        "et"    -> pure DictEt
        "et-ee" -> pure DictEtEE
        "fr"    -> pure DictFr
        "fr-fr" -> pure DictFrFR
        "nl"    -> pure DictNl
        "nl-nl" -> pure DictNlNL
        _ -> fail "Invalid Dict"
instance ToJSON Dict where
    toJSON = String . \case
        DictCs   -> "cs"
        DictCsCZ -> "cs-cz"
        DictDe   -> "de"
        DictDeDE -> "de-de"
        DictEn   -> "en"
        DictEnGB -> "en-gb"
        DictEnUS -> "en-us"
        DictEs   -> "es"
        DictEsES -> "es-es"
        DictEt   -> "et"
        DictEtEE -> "et-ee"
        DictFr   -> "fr"
        DictFrFR -> "fr-fr"
        DictNl   -> "nl"
        DictNlNL -> "nl-nl"

instance ToJSON ParaResult where
    toJSON o = object [
        "paragraph" .= paraResultPara o,
        "results"   .= paraResultResults o]

instance ToJSON Result where
    toJSON = \case
        ResultOk -> object [
            "status"  .= String "ok",
            "correct" .= True]
        ResultRoot root -> object [
            "status"  .= String "root",
            "correct" .= True,
            "root"    .= root]
        ResultCompound -> object [
            "status"  .= String "compound",
            "correct" .= True]
        ResultMiss orig offset misses -> object [
            "status"      .= String "miss",
            "correct"     .= False,
            "word"        .= orig,
            "offset"      .= offset,
            "suggestions" .= misses]
        ResultNone orig offset -> object [
            "status"  .= String "none",
            "correct" .= False,
            "word"    .= orig,
            "offset"  .= offset]
        _ -> Null
