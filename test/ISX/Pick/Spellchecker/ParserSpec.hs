module ISX.Pick.Spellchecker.ParserSpec (spec) where


import              ISX.Pick.Spellchecker.Parser
import              ISX.Test
import              Prelude                                 hiding  (get)


spec :: Spec
spec =
    describe "www.pavouk.tech" $
        it "/" $
            testPage "www.pavouk.tech/"


testPage :: Text -> IO ()
testPage url = do
    rock <- fRock url
    let texts = parse rock
    assertTextsLookup texts url
