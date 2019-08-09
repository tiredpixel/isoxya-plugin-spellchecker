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
                    ParaResult (ls !! 0) []]
            
            it "ok sentence" $ do
                let ls = [
                        "Out the top of St Agnes, a steep village on Cornwall’s north coast, sits a small utopia."]
                rs <- check' ls
                rs `shouldBe` [
                    ParaResult (ls !! 0) []]
            
            it "ok paragraphs" $ do
                let ls = [
                        "“We could have waited for one of the bigger wetsuit brands to figure this all out, but we didn’t, which I think is testament to how dedicated we are to our ethos of looking after our environment and bringing about positive change,” says Banks.",
                        "At any rate: the joy of surfing lies in never tiring of the pursuit of perfection, so their temperaments are well suited to their mission."]
                rs <- check' ls
                rs `shouldBe` [
                    ParaResult (ls !! 0) [],
                    ParaResult (ls !! 1) []]
            
            it "miss typo" $ do
                let ls = [
                        "Out the top of St Agnes, a steeep village on Cornwall’s north coast, sits a small utopia."]
                rs <- check' ls
                rs `shouldBe` [
                    ParaResult (ls !! 0) [
                        ResultMiss "steeep" 28 ["steep","steppe","testee","step","teepee"]]]
            
            it "miss multiline" $ do
                let ls = [
                        "There is stil a lot more testing\nto go – next, Kay and other testers will wear the suit with tempurature sensors",
                        "for a more precize asessment of tHe heat situation."]
                rs <- check' ls
                rs `shouldBe` [
                    ParaResult (ls !! 0) [
                        ResultMiss "stil" 10 ["slit","stile","stilt","still","silt","sail","stir","soil","instil","pistil","distil"],
                        ResultMiss "tempurature" 94 ["temperature","tempura","premature","maturate"]],
                    ParaResult (ls !! 1) [
                        ResultMiss "precize" 12 ["precise","prize"],
                        ResultMiss "asessment" 20 ["assessment","amassment","easement","assent"],
                        ResultMiss "tHe" 33 ["t He","the","tee","tie","toe","he","thee","then","them","they","she","thy"]]]
            
            it "miss interspersed" $ do
                let ls = [
                        "There is stil a lot more testing\nto go – next, Kay",
                        "and other testers will wear",
                        "the suit with temperature sensors",
                        "for a more precize asessment of tHe heat situation."]
                rs <- check' ls
                rs `shouldBe` [
                    ParaResult (ls !! 0) [
                        ResultMiss "stil" 10 ["slit","stile","stilt","still","silt","sail","stir","soil","instil","pistil","distil"]],
                    ParaResult (ls !! 1) [],
                    ParaResult (ls !! 2) [],
                    ParaResult (ls !! 3) [
                        ResultMiss "precize" 12 ["precise","prize"],
                        ResultMiss "asessment" 20 ["assessment","amassment","easement","assent"],
                        ResultMiss "tHe" 33 ["t He","the","tee","tie","toe","he","thee","then","them","they","she","thy"]]]
            
            it "miss control" $ do
                let ls = [
                        "*steeep"]
                rs <- check' ls
                rs `shouldBe` [
                    ParaResult (ls !! 0) [
                        ResultMiss "steeep" 2 ["steep","steppe","testee","step","teepee"]]]
            
            it "miss strange" $ do
                let ls = [
                        "aslkdflaasdfsdfhf"]
                rs <- check' ls
                rs `shouldBe` [
                    ParaResult (ls !! 0) [
                        ResultNone "aslkdflaasdfsdfhf" 1]]
        
        describe "multi" $ do
            let check' = check [DictEnGB, DictEnUS]
            
            it "ok en-gb" $ do
                let ls = [
                        "colours"]
                rs <- check' ls
                rs `shouldBe` [
                    ParaResult (ls !! 0) []]
            
            it "ok en-us" $ do
                let ls = [
                        "colors"]
                rs <- check' ls
                rs `shouldBe` [
                    ParaResult (ls !! 0) []]
        
        describe "cs" $ do
            -- https://www.radio.cz/cz/rubrika/ekonomika/cesko-ve-srovnani-se-zememi-eu-vynika-v-zamestnanosti-padesatniku
            
            let check' = check [DictCs]
            
            it "ok sentence" $ do
                let ls = [
                        "Česká republika si v rámci Evropské unie udržuje velmi dobrou kondici trhu práce."]
                rs <- check' ls
                rs `shouldBe` [
                    ParaResult (ls !! 0) []]
            
            it "miss sentence" $ do
                let ls = [
                        "Česka republika si v rámci Evropské uni udržuje velmi dobrou kondici trhu práce."]
                rs <- check' ls
                rs `shouldBe` [
                    ParaResult (ls !! 0) [
                        ResultMiss "uni" 37 ["inu","ni","unie","unii","unií","unci","učni","unik","upni","usni","unit","utni","uhni","ani","oni"]]]
        
        describe "cs-cz" $ do
            -- https://www.radio.cz/cz/rubrika/ekonomika/cesko-ve-srovnani-se-zememi-eu-vynika-v-zamestnanosti-padesatniku
            
            let check' = check [DictCsCZ]
            
            it "ok sentence" $ do
                let ls = [
                        "Česká republika si v rámci Evropské unie udržuje velmi dobrou kondici trhu práce."]
                rs <- check' ls
                rs `shouldBe` [
                    ParaResult (ls !! 0) []]
            
            it "miss sentence" $ do
                let ls = [
                        "Česka republika si v rámci Evropské uni udržuje velmi dobrou kondici trhu práce."]
                rs <- check' ls
                rs `shouldBe` [
                    ParaResult (ls !! 0) [
                        ResultMiss "uni" 37 ["inu","ni","unie","unii","unií","unci","učni","unik","upni","usni","unit","utni","uhni","ani","oni"]]]
        
        describe "de" $ do
            -- https://www.spiegel.de/wissenschaft/natur/polarstern-laeuft-fuer-klima-mission-richtung-arktis-aus-a-1280998.html
            
            let check' = check [DictDe]
            
            it "ok sentence" $ do
                let ls = [
                        "Die 140 Millionen Euro teure Expedition Richtung Norden beginnt dann am 20. September. Sie führt das Team von Norwegen entlang der sibirischen Küste Richtung Pol. Vor Ort werden Messungen im Meerwasser, im Eis und in der Atmosphäre vorgenommen."]
                rs <- check' ls
                rs `shouldBe` [
                    ParaResult (ls !! 0) []]
            
            it "miss sentence" $ do
                let ls = [
                        "Die 140 millionen Euro teure Expedition Richtung Norden beginnt dan am 20. September. Sie fuhrt das Team von Norwegen entlang der sibirischen Küste Richtung Pol. Vor Ort werden Messungen im Meerwaßer, im Eis und in der Atmosphäre vorgenommen."]
                rs <- check' ls
                rs `shouldBe` [
                    ParaResult (ls !! 0) [
                        ResultMiss "millionen" 9 ["Millionen","millionen-","-millionen","Billionen"],
                        ResultMiss "dan" 65 ["da","an","dann","dran","dank","den","das","dar","ran","man","van","San","Jan","Pan","Fan"],
                        ResultMiss "Meerwaßer" 191 ["Meerwasser","Meterware"]]]
        
        describe "de-de" $ do
            -- https://www.spiegel.de/wissenschaft/natur/polarstern-laeuft-fuer-klima-mission-richtung-arktis-aus-a-1280998.html
            
            let check' = check [DictDeDE]
            
            it "ok sentence" $ do
                let ls = [
                        "Die 140 Millionen Euro teure Expedition Richtung Norden beginnt dann am 20. September. Sie führt das Team von Norwegen entlang der sibirischen Küste Richtung Pol. Vor Ort werden Messungen im Meerwasser, im Eis und in der Atmosphäre vorgenommen."]
                rs <- check' ls
                rs `shouldBe` [
                    ParaResult (ls !! 0) []]
            
            it "miss sentence" $ do
                let ls = [
                        "Die 140 millionen Euro teure Expedition Richtung Norden beginnt dan am 20. September. Sie fuhrt das Team von Norwegen entlang der sibirischen Küste Richtung Pol. Vor Ort werden Messungen im Meerwaßer, im Eis und in der Atmosphäre vorgenommen."]
                rs <- check' ls
                rs `shouldBe` [
                    ParaResult (ls !! 0) [
                        ResultMiss "millionen" 9 ["Millionen","millionen-","-millionen","Billionen"],
                        ResultMiss "dan" 65 ["da","an","dann","dran","dank","den","das","dar","ran","man","van","San","Jan","Pan","Fan"],
                        ResultMiss "Meerwaßer" 191 ["Meerwasser","Meterware"]]]
        
        describe "en" $ do
            -- https://www.theguardian.com/environment/2019/aug/08/climate-crisis-reducing-lands-ability-to-sustain-humanity-says-ipcc
            
            let check' = check [DictEn]
            
            it "ok sentence" $ do
                let ls = [
                        "Global heating is increasing droughts, soil erosion and wildfires while diminishing crop yields in the tropics and thawing permafrost near the poles, says the report by the Intergovernmental Panel on Climate Change."]
                rs <- check' ls
                rs `shouldBe` [
                    ParaResult (ls !! 0) []]
            
            it "miss sentence" $ do
                let ls = [
                        "GloBal heeting is increesing droughts, soil erosion and wildfires whyle diminishing crop yields in the tropics and thawing permafrost near the Poles, says the report by the Intergovernmental Panel on Climate Change."]
                rs <- check' ls
                rs `shouldBe` [
                    ParaResult (ls !! 0) [
                        ResultMiss "GloBal" 1 ["Global","Glob al","Glob-al"],
                        ResultMiss "heeting" 8 ["sheeting","heating","heeling","heeding","meeting","hefting","greeting"],
                        ResultMiss "increesing" 19 ["increasing","screening","resining","cresting","resisting"],
                        ResultMiss "whyle" 67 ["while","whale","whole","why le","why-le","Whaley"]]]
            
            it "ok en-gb" $ do
                let ls = [
                        "colours"]
                rs <- check' ls
                rs `shouldBe` [
                    ParaResult (ls !! 0) []]
            
            it "ok en-us" $ do
                let ls = [
                        "colors"]
                rs <- check' ls
                rs `shouldBe` [
                    ParaResult (ls !! 0) []]
        
        describe "en-gb" $ do
            let check' = check [DictEnGB]
            
            it "ok en-gb" $ do
                let ls = [
                        "colours"]
                rs <- check' ls
                rs `shouldBe` [
                    ParaResult (ls !! 0) []]
            
            it "miss en-us" $ do
                let ls = [
                        "colors"]
                rs <- check' ls
                rs `shouldBe` [
                    ParaResult (ls !! 0) [
                        ResultMiss "colors" 1 ["colours","colons","col ors","col-ors","Coors","Corso"]]]
        
        describe "en-us" $ do
            let check' = check [DictEnUS]
            
            it "ok en-us" $ do
                let ls = [
                        "colors"]
                rs <- check' ls
                rs `shouldBe` [
                    ParaResult (ls !! 0) []]
            
            it "miss en-gb" $ do
                let ls = [
                        "colours"]
                rs <- check' ls
                rs `shouldBe` [
                    ParaResult (ls !! 0) [
                        ResultMiss "colours" 1 ["colors","co lours","co-lours","col ours","col-ours","coursers","courser","courses","colossus","colossal","callous"]]]
        
        describe "es" $ do
            -- https://elpais.com/sociedad/2019/08/07/actualidad/1565193502_273906.html
            
            let check' = check [DictEs]
            
            it "ok sentence" $ do
                let ls = [
                        "El planeta necesita un cambio del modelo alimentario para combatir la crisis climática"]
                rs <- check' ls
                rs `shouldBe` [
                    ParaResult (ls !! 0) []]
            
            it "miss sentence" $ do
                let ls = [
                        "El planeta necesita un cambio del modelo alementario paar combatir la crisis climatica"]
                rs <- check' ls
                rs `shouldBe` [
                    ParaResult (ls !! 0) [
                        ResultMiss "alementario" 42 ["alimentario","suplementario","complementario","parlamentario","argumentario"],
                        ResultMiss "paar" 54 ["para","par","parar","pasar","papar","pagar","pajar","payar","piar","puar"],
                        ResultMiss "climatica" 78 ["climática","climatice","climatiza","climaticé"]]]
        
        describe "es-es" $ do
            -- https://elpais.com/sociedad/2019/08/07/actualidad/1565193502_273906.html
            
            let check' = check [DictEsES]
            
            it "ok sentence" $ do
                let ls = [
                        "El planeta necesita un cambio del modelo alimentario para combatir la crisis climática"]
                rs <- check' ls
                rs `shouldBe` [
                    ParaResult (ls !! 0) []]
            
            it "miss sentence" $ do
                let ls = [
                        "El planeta necesita un cambio del modelo alementario paar combatir la crisis climatica"]
                rs <- check' ls
                rs `shouldBe` [
                    ParaResult (ls !! 0) [
                        ResultMiss "alementario" 42 ["alimentario","suplementario","complementario","parlamentario","argumentario"],
                        ResultMiss "paar" 54 ["para","par","parar","pasar","papar","pagar","pajar","payar","piar","puar"],
                        ResultMiss "climatica" 78 ["climática","climatice","climatiza","climaticé"]]]
        
        describe "et" $ do
            -- https://www.err.ee/968561/venemaa-militaarobjektil-kargatas-plahvatus-kiirgustase-hetkeks-tousis
            
            let check' = check [DictEt]
            
            it "ok sentence" $ do
                let ls = [
                        "\"Vedelkütusega raketimootori katsetuse ajal toimus plahvatus ja seade võttis tuld,\" ütles ministeerium."]
                rs <- check' ls
                rs `shouldBe` [
                    ParaResult (ls !! 0) []]
            
            it "miss sentence" $ do
                let ls = [
                        "\"Vedelkutusega raketimootori katsetuse ajal toiimus plahvatus ja seade vottis tuld,\" ütles ministeerium."]
                rs <- check' ls
                rs `shouldBe` [
                    ParaResult (ls !! 0) [
                        ResultMiss "Vedelkutusega" 2 ["VEdelkutusega","Edelkutusega","Aedelkutusega","Eedelkutusega","Tedelkutusega","Dedelkutusega","Medelkutusega","Redelkutusega","Õedelkutusega","Vedeldutusega","Vedelktuusega","Vedelkuutsega","Vedelkutsuega","Vedeldusega","Vedrutusega"],
                        ResultMiss "toiimus" 45 ["toimus","toismui","toiaimus","toieimus","toisimus","toiilmus","toiuimus","toioimus","toimimus","toiihmus","toihimus","toiäimus","toiimbus","toimunus","toitumus"],
                        ResultMiss "vottis" 72 ["vettis","voltis","nottis","kottis","võttis","vtotis","votits","vttois","tavotis"]]]
        
        describe "et-ee" $ do
            -- https://www.err.ee/968561/venemaa-militaarobjektil-kargatas-plahvatus-kiirgustase-hetkeks-tousis
            
            let check' = check [DictEtEE]
            
            it "ok sentence" $ do
                let ls = [
                        "\"Vedelkütusega raketimootori katsetuse ajal toimus plahvatus ja seade võttis tuld,\" ütles ministeerium."]
                rs <- check' ls
                rs `shouldBe` [
                    ParaResult (ls !! 0) []]
            
            it "miss sentence" $ do
                let ls = [
                        "\"Vedelkutusega raketimootori katsetuse ajal toiimus plahvatus ja seade vottis tuld,\" ütles ministeerium."]
                rs <- check' ls
                rs `shouldBe` [
                    ParaResult (ls !! 0) [
                        ResultMiss "Vedelkutusega" 2 ["VEdelkutusega","Edelkutusega","Aedelkutusega","Eedelkutusega","Tedelkutusega","Dedelkutusega","Medelkutusega","Redelkutusega","Õedelkutusega","Vedeldutusega","Vedelktuusega","Vedelkuutsega","Vedelkutsuega","Vedeldusega","Vedrutusega"],
                        ResultMiss "toiimus" 45 ["toimus","toismui","toiaimus","toieimus","toisimus","toiilmus","toiuimus","toioimus","toimimus","toiihmus","toihimus","toiäimus","toiimbus","toimunus","toitumus"],
                        ResultMiss "vottis" 72 ["vettis","voltis","nottis","kottis","võttis","vtotis","votits","vttois","tavotis"]]]
        
        describe "fr" $ do
            -- https://www.lemonde.fr/les-decodeurs/article/2019/08/08/journee-du-chat-des-toilettes-ou-de-la-resistance-comment-et-par-qui-sont-elles-decretees_5497728_4355770.html
            
            let check' = check [DictFr]
            
            it "ok sentence" $ do
                let ls = [
                        "Journées du chat, des toilettes ou de la Résistance : comment et par qui sont-elles décrétées ?"]
                rs <- check' ls
                rs `shouldBe` [
                    ParaResult (ls !! 0) []]
            
            it "miss sentence" $ do
                let ls = [
                        "Journees du chat, des toiletes ou de la Résistance : comment et par qui sont elles décrétées ?"]
                rs <- check' ls
                rs `shouldBe` [
                    ParaResult (ls !! 0) [
                        ResultMiss "Journees" 1 ["Journées","Ajournes","Séjournes","Journades"],
                        ResultMiss "toiletes" 23 ["toilettes","toilâtes","toile tes","toile-tes"]]]
        
        describe "fr-fr" $ do
            -- https://www.lemonde.fr/les-decodeurs/article/2019/08/08/journee-du-chat-des-toilettes-ou-de-la-resistance-comment-et-par-qui-sont-elles-decretees_5497728_4355770.html
            
            let check' = check [DictFrFR]
            
            it "ok sentence" $ do
                let ls = [
                        "Journées du chat, des toilettes ou de la Résistance : comment et par qui sont-elles décrétées ?"]
                rs <- check' ls
                rs `shouldBe` [
                    ParaResult (ls !! 0) []]
            
            it "miss sentence" $ do
                let ls = [
                        "Journees du chat, des toiletes ou de la Résistance : comment et par qui sont elles décrétées ?"]
                rs <- check' ls
                rs `shouldBe` [
                    ParaResult (ls !! 0) [
                        ResultMiss "Journees" 1 ["Journées","Ajournes","Séjournes","Journades"],
                        ResultMiss "toiletes" 23 ["toilettes","toilâtes","toile tes","toile-tes"]]]
        
        describe "nl" $ do
            -- https://www.nu.nl/buitenland/5976175/bijna-zevenhonderd-immigranten-vs-uitgezet-na-politie-invallen.html
            
            let check' = check [DictNl]
            
            it "ok sentence" $ do
                let ls = [
                        "Ongeveer 680 illegale inwoners van de Amerikaanse staat Mississippi zijn woensdagavond na politie-invallen gearresteerd."]
                rs <- check' ls
                rs `shouldBe` [
                    ParaResult (ls !! 0) []]
            
            it "miss sentence" $ do
                let ls = [
                        "Ongeveer 680 ilegale inwoners von de Amerikaanse staat Mississippi zijn woensdagavond na politie-invallen gearresteert."]
                rs <- check' ls
                rs `shouldBe` [
                    ParaResult (ls !! 0) [
                        ResultMiss "ilegale" 14 ["illegale","legale"],
                        ResultMiss "von" 31 ["con","bon","vin","on","vond","vont","vonk","Avon","Yvon","ven","vos","ion","van","vod","don"],
                        ResultMiss "gearresteert." 107 ["gearresteerd"]]]
        
        describe "nl-nl" $ do
            -- https://www.nu.nl/buitenland/5976175/bijna-zevenhonderd-immigranten-vs-uitgezet-na-politie-invallen.html
            
            let check' = check [DictNlNL]
            
            it "ok sentence" $ do
                let ls = [
                        "Ongeveer 680 illegale inwoners van de Amerikaanse staat Mississippi zijn woensdagavond na politie-invallen gearresteerd."]
                rs <- check' ls
                rs `shouldBe` [
                    ParaResult (ls !! 0) []]
            
            it "miss sentence" $ do
                let ls = [
                        "Ongeveer 680 ilegale inwoners von de Amerikaanse staat Mississippi zijn woensdagavond na politie-invallen gearresteert."]
                rs <- check' ls
                rs `shouldBe` [
                    ParaResult (ls !! 0) [
                        ResultMiss "ilegale" 14 ["illegale","legale"],
                        ResultMiss "von" 31 ["con","bon","vin","on","vond","vont","vonk","Avon","Yvon","ven","vos","ion","van","vod","don"],
                        ResultMiss "gearresteert." 107 ["gearresteerd"]]]
