module ISX.Plug.Spellchecker.Parser (parse) where


import           TPX.Com.Isoxya.PlugProc
import           Text.XML.HXT.Core
import qualified Data.Text               as T


parse :: PlugProcI -> [Text]
parse rx = T.strip . toText <$> texts
    where
        body = fromRight "" $ decodeUtf8' $ plugProcIBody rx
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
