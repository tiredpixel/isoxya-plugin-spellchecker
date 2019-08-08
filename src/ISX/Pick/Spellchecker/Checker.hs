module ISX.Pick.Spellchecker.Checker (
    Dict(..),
    Result(..),
    check
    ) where


import              System.Exit
import              System.Process.Text
import qualified    Data.Text                               as  T


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

data Result =
    ResultOk |
    ResultRoot WordRoot |
    ResultCompound |
    ResultMiss WordOriginal WordOffset [WordMiss] |
    ResultNone WordOriginal WordOffset
    deriving (Show, Eq)

type Results = [(Text, [Result])]

type WordMiss = Text

type WordOffset = Integer

type WordOriginal = Text

type WordRoot = Text

check :: [Dict] -> [Text] -> IO Results
check dicts texts = parse texts <$> hunspell dicts' texts
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
    return $ drop 1 $ lines out
    where
        -- ! terse-mode; ^ disable prefix [SEC]; replace inner newlines [SEC]
        texts' = "!" : (("^" <>) . T.replace "\n" " " <$> texts)
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

parse :: [Text] -> [Text] -> Results
parse texts results = zip texts results'
    where
        results' = (parseLine <$>) . filter ("" /=) <$>
            (lines <$> T.splitOn "\n\n" (unlines results))

parseLine :: Text -> Result
parseLine l = fromMaybe (error "invalid spellchecker output") $ case ctrl of
    "*" -> Just ResultOk
    "+" -> Just $ ResultRoot lRem
    "-" -> Just ResultCompound
    "&" -> parseLineMiss lRem
    "#" -> parseLineNone lRem
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
