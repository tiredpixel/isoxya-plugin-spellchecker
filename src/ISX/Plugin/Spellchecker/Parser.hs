module ISX.Plugin.Spellchecker.Parser (parse) where


import              Text.XML.HXT.Core
import qualified    Data.Text                               as  T
import qualified    PVK.Com.API.Resource.ISXPick            as  R


parse :: R.Rock -> [Text]
parse rock = T.strip . toText <$> texts
    where
        body = fromRight "" $ decodeUtf8' $ R.rockBody rock
        doc p = runLA (hread >>> p) $ toString body
        texts = doc $ deep (isTitle <+> isTagH <+> isTagP) >>>
            removeAllWhiteSpace //> getText


isTagH :: LA XmlTree XmlTree
isTagH = hasName "h1" <+> hasName "h2" <+> hasName "h3" <+>
    hasName "h4" <+> hasName "h5" <+> hasName "h6"

isTagP :: LA XmlTree XmlTree
isTagP = hasName "p"

isTitle :: LA XmlTree XmlTree
isTitle = hasName "title"
