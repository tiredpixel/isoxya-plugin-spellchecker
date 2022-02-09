module Isoxya.Plugin.Spellchecker.Checker (
    Dictionary(..),
    ParagraphResult(..),
    Result(..),
    check,
    ) where


import qualified Data.List           as L
import qualified Data.Text           as T
import           System.Exit
import           System.Process.Text


data Dictionary = DictionaryCs | DictionaryCsCz | DictionaryDe | DictionaryDeDe | DictionaryEn | DictionaryEnGb | DictionaryEnUs | DictionaryEs | DictionaryEsEs | DictionaryEt | DictionaryEtEe | DictionaryFr | DictionaryFrFr | DictionaryNl | DictionaryNlNl deriving
    ( Show
    )

type Paragraph = Text

data ParagraphResult = ParagraphResult
                         { paragraphResultParagraph :: Paragraph
                         , paragraphResultResults   :: [Result]
                         }
  deriving (Eq, Show)

data Result = ResultOk
            | ResultRoot WordRoot
            | ResultCompound
            | ResultMiss WordOriginal WordOffset [WordMiss]
            | ResultNone WordOriginal WordOffset
            | ResultSeparator
  deriving (Eq, Show)

type WordMiss = Text

type WordOffset = Integer

type WordOriginal = Text

type WordRoot = Text

check :: MonadIO m => [Dictionary] -> [Paragraph] -> m [ParagraphResult]
check dicts texts = parse texts <$> liftIO (hunspell dicts' texts)
    where
        dicts' = if null dicts
            then [dictDef]
            else dicts


dictDef :: Dictionary
dictDef = DictionaryEn

hunspell :: [Dictionary] -> [Text] -> IO [Text]
hunspell dicts texts = do
    (ExitSuccess, out, _) <- readProcessWithExitCode "hunspell" [
        "-d", intercalate "," dicts',
        "-a"
        ] (unlines texts')
    return $ drop 1 $ lines $ T.stripEnd out
    where
        -- not ! terse-mode, as that affects alignment of results when there
        -- are consecutive correct lines
        -- ^ disable prefix [SEC]
        -- replace inner newlines [SEC]
        texts' = ("^" <>) . T.replace "\n" " " <$> texts
        dicts' = concat $ hunspellDictionaries <$> dicts

hunspellDictionaries :: Dictionary -> [String]
hunspellDictionaries d = case d of
    DictionaryCs   -> ["cs_CZ"]
    DictionaryCsCz -> ["cs_CZ"]
    DictionaryDe   -> ["de_DE"]
    DictionaryDeDe -> ["de_DE"]
    DictionaryEn   -> ["en_GB", "en_US"]
    DictionaryEnGb -> ["en_GB"]
    DictionaryEnUs -> ["en_US"]
    DictionaryEs   -> ["es_ES"]
    DictionaryEsEs -> ["es_ES"]
    DictionaryEt   -> ["et_EE"]
    DictionaryEtEe -> ["et_EE"]
    DictionaryFr   -> ["fr_FR"]
    DictionaryFrFr -> ["fr_FR"]
    DictionaryNl   -> ["nl_NL"]
    DictionaryNlNl -> ["nl_NL"]

parse :: [Paragraph] -> [Text] -> [ParagraphResult]
parse texts results = zipWith ParagraphResult texts results'
    where
        results' = (filter isMistake <$>) <$>
            L.groupBy groupSep $ parseLine <$> results
        groupSep _ b = case b of
            ResultSeparator {} -> False
            _                  -> True
        isMistake r = case r of
            ResultMiss {} -> True
            ResultNone {} -> True
            _             -> False

parseLine :: Text -> Result
parseLine l = fromMaybe (error "invalid spellchecker output") $ case ctrl of
    "*" -> Just ResultOk
    "+" -> Just $ ResultRoot lRem
    "-" -> Just ResultCompound
    "&" -> parseLineMiss lRem
    "#" -> parseLineNone lRem
    ""  -> Just ResultSeparator
    _   -> Nothing
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
