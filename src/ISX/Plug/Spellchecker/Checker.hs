module ISX.Plug.Spellchecker.Checker (
    Dict(..),
    ParaResult(..),
    Result(..),
    check,
    ) where


import           System.Exit
import           System.Process.Text
import qualified Data.List           as L
import qualified Data.Text           as T


data Dict =
    DictCs |
    DictCsCZ |
    DictDe |
    DictDeDE |
    DictEn |
    DictEnGB |
    DictEnUS |
    DictEs |
    DictEsES |
    DictEt |
    DictEtEE |
    DictFr |
    DictFrFR |
    DictNl |
    DictNlNL
    deriving (Show)

type Para = Text

data ParaResult = ParaResult {
    paraResultPara    :: Para,
    paraResultResults :: [Result]
    } deriving (Show, Eq)

data Result =
    ResultOk |
    ResultRoot WordRoot |
    ResultCompound |
    ResultMiss WordOrig WordOffset [WordMiss] |
    ResultNone WordOrig WordOffset |
    ResultSep
    deriving (Show, Eq)

type WordMiss = Text

type WordOffset = Integer

type WordOrig = Text

type WordRoot = Text

check :: MonadIO m => [Dict] -> [Para] -> m [ParaResult]
check dicts texts = parse texts <$> liftIO (hunspell dicts' texts)
    where
        dicts' = if null dicts
            then [dictDef]
            else dicts


dictDef :: Dict
dictDef = DictEn

hunspell :: [Dict] -> [Text] -> IO [Text]
hunspell dicts texts = do
    (ExitSuccess, out, _) <- readProcessWithExitCode "hunspell" [
        "-d", intercalate "," dicts',
        "-a"
        ] (unlines texts')
    return $ drop 1 $ lines $ T.stripEnd out
    where
        -- not ! terse-mode, as that affects alignment of results, when there
        -- are consecutive correct lines
        -- ^ disable prefix [SEC]; replace inner newlines [SEC]
        texts' = ("^" <>) . T.replace "\n" " " <$> texts
        dicts' = concat $ hunspellDicts <$> dicts

hunspellDicts :: Dict -> [String]
hunspellDicts d = case d of
    DictCs   -> ["cs_CZ"]
    DictCsCZ -> ["cs_CZ"]
    DictDe   -> ["de_DE"]
    DictDeDE -> ["de_DE"]
    DictEn   -> ["en_GB", "en_US"]
    DictEnGB -> ["en_GB"]
    DictEnUS -> ["en_US"]
    DictEs   -> ["es_ES"]
    DictEsES -> ["es_ES"]
    DictEt   -> ["et_EE"]
    DictEtEE -> ["et_EE"]
    DictFr   -> ["fr_FR"]
    DictFrFR -> ["fr_FR"]
    DictNl   -> ["nl_NL"]
    DictNlNL -> ["nl_NL"]

parse :: [Para] -> [Text] -> [ParaResult]
parse texts results = zipWith ParaResult texts results'
    where
        results' = (filter isMistake <$>) <$>
            L.groupBy groupSep $ parseLine <$> results
        groupSep _ b = case b of
            ResultSep {} -> False
            _ -> True
        isMistake r = case r of
            ResultMiss {} -> True
            ResultNone {} -> True
            _ -> False

parseLine :: Text -> Result
parseLine l = fromMaybe (error "invalid spellchecker output") $ case ctrl of
    "*" -> Just ResultOk
    "+" -> Just $ ResultRoot lRem
    "-" -> Just ResultCompound
    "&" -> parseLineMiss lRem
    "#" -> parseLineNone lRem
    ""  -> Just ResultSep
    _ -> Nothing
    where
        (ctrl, lRem_) = T.breakOn " " l
        lRem = T.drop 1 lRem_

parseLineMiss :: Text -> Maybe Result
parseLineMiss lRem = do
    o' <- readMaybe $ toString o
    return $ ResultMiss w o' misses'
    where
        (meta, misses_) = T.breakOn ":" lRem
        [w, _, o] = words meta -- discard misses count
        misses' = T.splitOn ", " $ T.drop 2 misses_

parseLineNone :: Text -> Maybe Result
parseLineNone lRem = do
    o' <- readMaybe $ toString o
    return $ ResultNone w o'
    where
        (w, o) = T.breakOn " " lRem
