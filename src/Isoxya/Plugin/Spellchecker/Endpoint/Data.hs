module Isoxya.Plugin.Spellchecker.Endpoint.Data (
    create,
    ) where


import           Isoxya.Plugin.Spellchecker.Core
import           TiredPixel.Common.Isoxya.Processor
import           TiredPixel.Common.Isoxya.Snap.Processor ()
import qualified Data.Set                                   as S


create :: Handler b Spellchecker ()
create = do
    req_     <- getBoundedJSON' reqLim >>= validateJSON
    Just req <- runValidate req_
    let dicts = maybe [] configDictionaries (reparseConfig req)
    let texts = take parasLim $ parse req
    results <- liftIO $ check dicts texts
    let results' = filter isMistake results
    writeJSON ProcessorO {
        processorOData = toJSON results',
        processorOURLs = S.empty}
    where
        isMistake = not . null . paragraphResultResults
        parasLim = 100 -- paragraphs
        reqLim = 2097152 -- 2 MB = (1 + .5) * (4/3) MB


reparseConfig :: ProcessorI -> Maybe Config
reparseConfig = decode . encode . processorIMetaConfig . processorIMeta
