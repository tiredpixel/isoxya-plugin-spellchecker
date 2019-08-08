module ISX.Pick.Spellchecker.CheckerSpec (spec) where


import              ISX.Pick.Spellchecker.Checker
import              ISX.Test
import              Prelude                                 hiding  (get)
import              Relude.Unsafe                           ((!!))


{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}


spec :: Spec
spec =
    -- mistakes added and formatting changed purposely
    describe "check" $ do
        
        describe "default" $ do
            -- https://www.theguardian.com/world/2019/aug/08/the-sustainable-surfer-meet-the-man-behind-the-worlds-first-fully-recyclable-wetsuit
            
            let check' = check []
            
            it "ok empty" $ do
                rs <- check' []
                rs `shouldBe` []
            
            it "ok word" $ do
                let ls = [
                        "biodegradable"]
                rs <- check' ls
                rs `shouldBe` [
                    (ls !! 0, [])]
            
            it "ok sentence" $ do
                let ls = [
                        "Out the top of St Agnes, a steep village on Cornwall’s north coast, sits a small utopia."]
                rs <- check' ls
                rs `shouldBe` [
                    (ls !! 0, [])]
            
            it "ok paragraphs" $ do
                let ls = [
                        "“We could have waited for one of the bigger wetsuit brands to figure this all out, but we didn’t, which I think is testament to how dedicated we are to our ethos of looking after our environment and bringing about positive change,” says Banks.",
                        "At any rate: the joy of surfing lies in never tiring of the pursuit of perfection, so their temperaments are well suited to their mission."]
                rs <- check' ls
                rs `shouldBe` [
                    (ls !! 0, []),
                    (ls !! 1, [])]
            
            it "miss typo" $ do
                let ls = [
                        "Out the top of St Agnes, a steeep village on Cornwall’s north coast, sits a small utopia."]
                rs <- check' ls
                rs `shouldBe` [
                    (ls !! 0, [
                        ResultMiss "steeep" 28 ["steep","steppe","testee","step","teepee"]])]
            
            it "miss multiline" $ do
                let ls = [
                        "There is stil a lot more testing\nto go – next, Kay and other testers will wear the suit with tempurature sensors",
                        "for a more precize asessment of tHe heat situation."]
                rs <- check' ls
                rs `shouldBe` [
                    (ls !! 0, [
                        ResultMiss "stil" 10 ["slit","stile","stilt","still","silt","sail","stir","soil","instil","pistil","distil"],
                        ResultMiss "tempurature" 94 ["temperature","tempura","premature","maturate"]]),
                    (ls !! 1, [
                        ResultMiss "precize" 12 ["precise","prize"],
                        ResultMiss "asessment" 20 ["assessment","amassment","easement","assent"],
                        ResultMiss "tHe" 33 ["t He","the","tee","tie","toe","he","thee","then","them","they","she","thy"]])]
            
            it "miss control" $ do
                let ls = [
                        "*steeep"]
                rs <- check' ls
                rs `shouldBe` [
                    (ls !! 0, [
                        ResultMiss "steeep" 2 ["steep","steppe","testee","step","teepee"]])]
            
            it "miss strange" $ do
                let ls = [
                        "aslkdflaasdfsdfhf"]
                rs <- check' ls
                rs `shouldBe` [
                    (ls !! 0, [
                        ResultNone "aslkdflaasdfsdfhf" 1])]
        
        describe "multi" $ do
            let check' = check [DictEnGB, DictEnUS]
            
            it "ok en-gb" $ do
                let ls = [
                        "colours"]
                rs <- check' ls
                rs `shouldBe` [
                    (ls !! 0, [])]
            
            it "ok en-us" $ do
                let ls = [
                        "colors"]
                rs <- check' ls
                rs `shouldBe` [
                    (ls !! 0, [])]
        
        describe "en" $ do
            -- https://www.theguardian.com/environment/2019/aug/08/climate-crisis-reducing-lands-ability-to-sustain-humanity-says-ipcc
            
            let check' = check [DictEn]
            
            it "ok sentence" $ do
                let ls = [
                        "Global heating is increasing droughts, soil erosion and wildfires while diminishing crop yields in the tropics and thawing permafrost near the poles, says the report by the Intergovernmental Panel on Climate Change."]
                rs <- check' ls
                rs `shouldBe` [
                    (ls !! 0, [])]
            
            it "miss sentence" $ do
                let ls = [
                        "GloBal heeting is increesing droughts, soil erosion and wildfires whyle diminishing crop yields in the tropics and thawing permafrost near the Poles, says the report by the Intergovernmental Panel on Climate Change."]
                rs <- check' ls
                rs `shouldBe` [
                    (ls !! 0, [
                        ResultMiss "GloBal" 1 ["Global","Glob al","Glob-al"],
                        ResultMiss "heeting" 8 ["sheeting","heating","heeling","heeding","meeting","hefting","greeting"],
                        ResultMiss "increesing" 19 ["increasing","screening","resining","cresting","resisting"],
                        ResultMiss "whyle" 67 ["while","whale","whole","why le","why-le","Whaley"]])]
            
            it "ok en-gb" $ do
                let ls = [
                        "colours"]
                rs <- check' ls
                rs `shouldBe` [
                    (ls !! 0, [])]
            
            it "ok en-us" $ do
                let ls = [
                        "colors"]
                rs <- check' ls
                rs `shouldBe` [
                    (ls !! 0, [])]
        
        describe "en-gb" $ do
            let check' = check [DictEnGB]
            
            it "ok en-gb" $ do
                let ls = [
                        "colours"]
                rs <- check' ls
                rs `shouldBe` [
                    (ls !! 0, [])]
            
            it "miss en-us" $ do
                let ls = [
                        "colors"]
                rs <- check' ls
                rs `shouldBe` [
                    (ls !! 0, [
                        ResultMiss "colors" 1 ["colours","colons","col ors","col-ors","Coors","Corso"]])]
        
        describe "en-us" $ do
            let check' = check [DictEnUS]
            
            it "ok en-us" $ do
                let ls = [
                        "colors"]
                rs <- check' ls
                rs `shouldBe` [
                    (ls !! 0, [])]
            
            it "miss en-gb" $ do
                let ls = [
                        "colours"]
                rs <- check' ls
                rs `shouldBe` [
                    (ls !! 0, [
                        ResultMiss "colours" 1 ["colors","co lours","co-lours","col ours","col-ours","coursers","courser","courses","colossus","colossal","callous"]])]
