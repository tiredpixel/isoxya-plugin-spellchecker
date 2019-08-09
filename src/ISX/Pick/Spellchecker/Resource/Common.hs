{-# OPTIONS_GHC -fno-warn-orphans #-}


module ISX.Pick.Spellchecker.Resource.Common (
    Apex(..)
    ) where


import              Data.Aeson                              hiding (Result)
import              Data.Time.Clock                         (UTCTime)
import              ISX.Pick.Spellchecker.Checker


data Apex = Apex {
    apexTNow    :: UTCTime,
    apexVersion :: Text
    } deriving (Show)
instance ToJSON Apex where
    toJSON o = object [
        "t_now"   .= apexTNow o,
        "version" .= apexVersion o]


instance ToJSON ParaResult where
    toJSON o = object [
        "paragraph" .= paraResultPara o,
        "results"   .= paraResultResults o]

instance ToJSON Result where
    toJSON o = case o of
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
