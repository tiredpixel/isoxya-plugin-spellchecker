module Isoxya.Plugin.Spellchecker.CheckerSpec (spec) where


import           Isoxya.Plugin.Spellchecker.Test


spec :: Spec
spec = snapSpellchecker $
    -- mistakes added and formatting changed purposely
    describe "check" $ do

        describe "default" $ do
            -- https://www.theguardian.com/world/2019/aug/08/the-sustainable-surfer-meet-the-man-behind-the-worlds-first-fully-recyclable-wetsuit

            let check' = check []

            it "ok empty" $ do
                rs <- check' []
                rs `shouldBeList` []

            it "ok word" $ do
                let l0 = "biodegradable"
                rs <- check' [l0]
                rs `shouldBeList` [
                    ParagraphResult l0 []]

            it "ok sentence" $ do
                let l0 = "Out the top of St Agnes, a steep village on Cornwall’s north coast, sits a small utopia."
                rs <- check' [l0]
                rs `shouldBeList` [
                    ParagraphResult l0 []]

            it "ok paragraphs" $ do
                let l0 = "“We could have waited for one of the bigger wetsuit brands to figure this all out, but we didn’t, which I think is testament to how dedicated we are to our ethos of looking after our environment and bringing about positive change,” says Banks."
                let l1 = "At any rate: the joy of surfing lies in never tiring of the pursuit of perfection, so their temperaments are well suited to their mission."
                rs <- check' [l0, l1]
                rs `shouldBeList` [
                    ParagraphResult l0 [],
                    ParagraphResult l1 []]

            it "miss typo" $ do
                let l0 = "Out the top of St Agnes, a steeep village on Cornwall’s north coast, sits a small utopia."
                rs <- check' [l0]
                rs `shouldBeList` [
                    ParagraphResult l0 [
                        ResultMiss "steeep" 28 ["steep","steppe","testee","step","teepee"]]]

            it "miss multiline" $ do
                let l0 = "There is stil a lot more testing\nto go – next, Kay and other testers will wear the suit with tempurature sensors"
                let l1 = "for a more precize asessment of tHe heat situation."
                rs <- check' [l0, l1]
                rs `shouldBeList` [
                    ParagraphResult l0 [
                        ResultMiss "stil" 10 ["slit","stile","stilt","still","silt","sail","stir","soil","instil","distil","pistil"],
                        ResultMiss "tempurature" 94 ["temperature","tempura","premature","maturate"]],
                    ParagraphResult l1 [
                        ResultMiss "precize" 12 ["precise","prize"],
                        ResultMiss "asessment" 20 ["assessment","amassment","easement","assent"],
                        ResultMiss "tHe" 33 ["t He","the","tee","tie","toe","The","he","thee","then","them","they","she","thy","Che"]]]

            it "miss interspersed" $ do
                let l0 = "There is stil a lot more testing\nto go – next, Kay"
                let l1 = "and other testers will wear"
                let l2 = "the suit with temperature sensors"
                let l3 = "for a more precize asessment of tHe heat situation."
                rs <- check' [l0, l1, l2, l3]
                rs `shouldBeList` [
                    ParagraphResult l0 [
                        ResultMiss "stil" 10 ["slit","stile","stilt","still","silt","sail","stir","soil","instil","distil","pistil"]],
                    ParagraphResult l1 [],
                    ParagraphResult l2 [],
                    ParagraphResult l3 [
                        ResultMiss "precize" 12 ["precise","prize"],
                        ResultMiss "asessment" 20 ["assessment","amassment","easement","assent"],
                        ResultMiss "tHe" 33 ["t He","the","tee","tie","toe","The","he","thee","then","them","they","she","thy","Che"]]]

            it "miss control" $ do
                let l0 = "*steeep"
                rs <- check' [l0]
                rs `shouldBeList` [
                    ParagraphResult l0 [
                        ResultMiss "steeep" 2 ["steep","steppe","testee","step","teepee"]]]

            it "miss strange" $ do
                let l0 = "aslkdflaasdfsdfhf"
                rs <- check' [l0]
                rs `shouldBeList` [
                    ParagraphResult l0 [
                        ResultNone "aslkdflaasdfsdfhf" 1]]

        describe "multi" $ do
            let check' = check [DictionaryEnGb, DictionaryEnUs]

            it "ok en-gb" $ do
                let l0 = "colours"
                rs <- check' [l0]
                rs `shouldBeList` [
                    ParagraphResult l0 []]

            it "ok en-us" $ do
                let l0 = "colors"
                rs <- check' [l0]
                rs `shouldBeList` [
                    ParagraphResult l0 []]

        describe "cs" $ do
            -- https://www.radio.cz/cz/rubrika/ekonomika/cesko-ve-srovnani-se-zememi-eu-vynika-v-zamestnanosti-padesatniku

            let check' = check [DictionaryCs]

            it "ok sentence" $ do
                let l0 = "Česká republika si v rámci Evropské unie udržuje velmi dobrou kondici trhu práce."
                rs <- check' [l0]
                rs `shouldBeList` [
                    ParagraphResult l0 []]

            it "miss sentence" $ do
                let l0 = "Česka republika si v rámci Evropské uni udržuje velmi dobrou kondici trhu práce."
                rs <- check' [l0]
                rs `shouldBeList` [
                    ParagraphResult l0 [
                        ResultMiss "uni" 37 ["inu","ni","unie","unii","unií","unci","učni","unik","upni","usni","unit","utni","uhni","ani","oni"]]]

        describe "cs-cz" $ do
            -- https://www.radio.cz/cz/rubrika/ekonomika/cesko-ve-srovnani-se-zememi-eu-vynika-v-zamestnanosti-padesatniku

            let check' = check [DictionaryCsCz]

            it "ok sentence" $ do
                let l0 = "Česká republika si v rámci Evropské unie udržuje velmi dobrou kondici trhu práce."
                rs <- check' [l0]
                rs `shouldBeList` [
                    ParagraphResult l0 []]

            it "miss sentence" $ do
                let l0 = "Česka republika si v rámci Evropské uni udržuje velmi dobrou kondici trhu práce."
                rs <- check' [l0]
                rs `shouldBeList` [
                    ParagraphResult l0 [
                        ResultMiss "uni" 37 ["inu","ni","unie","unii","unií","unci","učni","unik","upni","usni","unit","utni","uhni","ani","oni"]]]

        describe "de" $ do
            -- https://www.spiegel.de/wissenschaft/natur/polarstern-laeuft-fuer-klima-mission-richtung-arktis-aus-a-1280998.html

            let check' = check [DictionaryDe]

            it "ok sentence" $ do
                let l0 = "Die 140 Millionen Euro teure Expedition Richtung Norden beginnt dann am 20. September. Sie führt das Team von Norwegen entlang der sibirischen Küste Richtung Pol. Vor Ort werden Messungen im Meerwasser, im Eis und in der Atmosphäre vorgenommen."
                rs <- check' [l0]
                rs `shouldBeList` [
                    ParagraphResult l0 []]

            it "miss sentence" $ do
                let l0 = "Die 140 millionen Euro teure Expedition Richtung Norden beginnt dan am 20. September. Sie fuhrt das Team von Norwegen entlang der sibirischen Küste Richtung Pol. Vor Ort werden Messungen im Meerwaßer, im Eis und in der Atmosphäre vorgenommen."
                rs <- check' [l0]
                rs `shouldBeList` [
                    ParagraphResult l0 [
                        ResultMiss "millionen" 9 ["Millionen","millionen-","-millionen","Billionen","millionsten","millionste"],
                        ResultMiss "dan" 65 ["da","an","dann","dran","dank","den","das","dar","ran","man","van","San","Jan","Pan","Fan"],
                        ResultMiss "Meerwaßer" 191 ["Meerwasser"]]]

        describe "de-de" $ do
            -- https://www.spiegel.de/wissenschaft/natur/polarstern-laeuft-fuer-klima-mission-richtung-arktis-aus-a-1280998.html

            let check' = check [DictionaryDeDe]

            it "ok sentence" $ do
                let l0 = "Die 140 Millionen Euro teure Expedition Richtung Norden beginnt dann am 20. September. Sie führt das Team von Norwegen entlang der sibirischen Küste Richtung Pol. Vor Ort werden Messungen im Meerwasser, im Eis und in der Atmosphäre vorgenommen."
                rs <- check' [l0]
                rs `shouldBeList` [
                    ParagraphResult l0 []]

            it "miss sentence" $ do
                let l0 = "Die 140 millionen Euro teure Expedition Richtung Norden beginnt dan am 20. September. Sie fuhrt das Team von Norwegen entlang der sibirischen Küste Richtung Pol. Vor Ort werden Messungen im Meerwaßer, im Eis und in der Atmosphäre vorgenommen."
                rs <- check' [l0]
                rs `shouldBeList` [
                    ParagraphResult l0 [
                        ResultMiss "millionen" 9 ["Millionen","millionen-","-millionen","Billionen","millionsten","millionste"],
                        ResultMiss "dan" 65 ["da","an","dann","dran","dank","den","das","dar","ran","man","van","San","Jan","Pan","Fan"],
                        ResultMiss "Meerwaßer" 191 ["Meerwasser"]]]

        describe "en" $ do
            -- https://www.theguardian.com/environment/2019/aug/08/climate-crisis-reducing-lands-ability-to-sustain-humanity-says-ipcc

            let check' = check [DictionaryEn]

            it "ok sentence" $ do
                let l0 = "Global heating is increasing droughts, soil erosion and wildfires while diminishing crop yields in the tropics and thawing permafrost near the poles, says the report by the Intergovernmental Panel on Climate Change."
                rs <- check' [l0]
                rs `shouldBeList` [
                    ParagraphResult l0 []]

            it "miss sentence" $ do
                let l0 = "GloBal heeting is increesing droughts, soil erosion and wildfires whyle diminishing crop yields in the tropics and thawing permafrost near the Poles, says the report by the Intergovernmental Panel on Climate Change."
                rs <- check' [l0]
                rs `shouldBeList` [
                    ParagraphResult l0 [
                        ResultMiss "GloBal" 1 ["Global","Glob al","Glob-al"],
                        ResultMiss "heeting" 8 ["sheeting","heating","heeling","heeding","meeting","hefting","Weeting","greeting"],
                        ResultMiss "increesing" 19 ["increasing","screening","cresting","resisting","pressing"],
                        ResultMiss "whyle" 67 ["Whyle","while","whale","whole","why le","why-le"]]]

            it "ok en-gb" $ do
                let l0 = "colours"
                rs <- check' [l0]
                rs `shouldBeList` [
                    ParagraphResult l0 []]

            it "ok en-us" $ do
                let l0 = "colors"
                rs <- check' [l0]
                rs `shouldBeList` [
                    ParagraphResult l0 []]

        describe "en-gb" $ do
            let check' = check [DictionaryEnGb]

            it "ok en-gb" $ do
                let l0 = "colours"
                rs <- check' [l0]
                rs `shouldBeList` [
                    ParagraphResult l0 []]

            it "miss en-us" $ do
                let l0 = "colors"
                rs <- check' [l0]
                rs `shouldBeList` [
                    ParagraphResult l0 [
                        ResultMiss "colors" 1 ["colours","colons","col ors","col-ors","collators"]]]

        describe "en-us" $ do
            let check' = check [DictionaryEnUs]

            it "ok en-us" $ do
                let l0 = "colors"
                rs <- check' [l0]
                rs `shouldBeList` [
                    ParagraphResult l0 []]

            it "miss en-gb" $ do
                let l0 = "colours"
                rs <- check' [l0]
                rs `shouldBeList` [
                    ParagraphResult l0 [
                        ResultMiss "colours" 1 ["colors","co lours","co-lours","col ours","col-ours","coursers"]]]

        describe "es" $ do
            -- https://elpais.com/sociedad/2019/08/07/actualidad/1565193502_273906.html

            let check' = check [DictionaryEs]

            it "ok sentence" $ do
                let l0 = "El planeta necesita un cambio del modelo alimentario para combatir la crisis climática"
                rs <- check' [l0]
                rs `shouldBeList` [
                    ParagraphResult l0 []]

            it "miss sentence" $ do
                let l0 = "El planeta necesita un cambio del modelo alementario paar combatir la crisis climatica"
                rs <- check' [l0]
                rs `shouldBeList` [
                    ParagraphResult l0 [
                        ResultMiss "alementario" 42 ["alimentario","suplementario","complementario","parlamentario","argumentario"],
                        ResultMiss "paar" 54 ["para","par","parar","pasar","papar","pagar","pajar","payar","piar","puar"],
                        ResultMiss "climatica" 78 ["climática","climatice","climatiza","climaticé"]]]

        describe "es-es" $ do
            -- https://elpais.com/sociedad/2019/08/07/actualidad/1565193502_273906.html

            let check' = check [DictionaryEsEs]

            it "ok sentence" $ do
                let l0 ="El planeta necesita un cambio del modelo alimentario para combatir la crisis climática"
                rs <- check' [l0]
                rs `shouldBeList` [
                    ParagraphResult l0 []]

            it "miss sentence" $ do
                let l0 = "El planeta necesita un cambio del modelo alementario paar combatir la crisis climatica"
                rs <- check' [l0]
                rs `shouldBeList` [
                    ParagraphResult l0 [
                        ResultMiss "alementario" 42 ["alimentario","suplementario","complementario","parlamentario","argumentario"],
                        ResultMiss "paar" 54 ["para","par","parar","pasar","papar","pagar","pajar","payar","piar","puar"],
                        ResultMiss "climatica" 78 ["climática","climatice","climatiza","climaticé"]]]

        describe "et" $ do
            -- https://www.err.ee/968561/venemaa-militaarobjektil-kargatas-plahvatus-kiirgustase-hetkeks-tousis

            let check' = check [DictionaryEt]

            it "ok sentence" $ do
                let l0 = "\"Vedelkütusega raketimootori katsetuse ajal toimus plahvatus ja seade võttis tuld,\" ütles ministeerium."
                rs <- check' [l0]
                rs `shouldBeList` [
                    ParagraphResult l0 []]

            it "miss sentence" $ do
                let l0 = "\"Vedelkutusega raketimootori katsetuse ajal toiimus plahvatus ja seade vottis tuld,\" ütles ministeerium."
                rs <- check' [l0]
                rs `shouldBeList` [
                    ParagraphResult l0 [
                        ResultMiss "Vedelkutusega" 2 ["VEdelkutusega","Edelkutusega","Aedelkutusega","Eedelkutusega","Tedelkutusega","Dedelkutusega","Medelkutusega","Redelkutusega","Õedelkutusega","Vedeldutusega","Vedelktuusega","Vedelkuutsega","Vedelkutsuega","Vedeldusega","Vedrutusega"],
                        ResultMiss "toiimus" 45 ["toimus","toismui","toiaimus","toieimus","toisimus","toiilmus","toiuimus","toioimus","toimimus","toiihmus","toihimus","toiäimus","toiimbus","toimunus","toitumus"],
                        ResultMiss "vottis" 72 ["vettis","voltis","nottis","kottis","võttis","vtotis","votits","vttois","tavotis"]]]

        describe "et-ee" $ do
            -- https://www.err.ee/968561/venemaa-militaarobjektil-kargatas-plahvatus-kiirgustase-hetkeks-tousis

            let check' = check [DictionaryEtEe]

            it "ok sentence" $ do
                let l0 = "\"Vedelkütusega raketimootori katsetuse ajal toimus plahvatus ja seade võttis tuld,\" ütles ministeerium."
                rs <- check' [l0]
                rs `shouldBeList` [
                    ParagraphResult l0 []]

            it "miss sentence" $ do
                let l0 = "\"Vedelkutusega raketimootori katsetuse ajal toiimus plahvatus ja seade vottis tuld,\" ütles ministeerium."
                rs <- check' [l0]
                rs `shouldBeList` [
                    ParagraphResult l0 [
                        ResultMiss "Vedelkutusega" 2 ["VEdelkutusega","Edelkutusega","Aedelkutusega","Eedelkutusega","Tedelkutusega","Dedelkutusega","Medelkutusega","Redelkutusega","Õedelkutusega","Vedeldutusega","Vedelktuusega","Vedelkuutsega","Vedelkutsuega","Vedeldusega","Vedrutusega"],
                        ResultMiss "toiimus" 45 ["toimus","toismui","toiaimus","toieimus","toisimus","toiilmus","toiuimus","toioimus","toimimus","toiihmus","toihimus","toiäimus","toiimbus","toimunus","toitumus"],
                        ResultMiss "vottis" 72 ["vettis","voltis","nottis","kottis","võttis","vtotis","votits","vttois","tavotis"]]]

        describe "fr" $ do
            -- https://www.lemonde.fr/les-decodeurs/article/2019/08/08/journee-du-chat-des-toilettes-ou-de-la-resistance-comment-et-par-qui-sont-elles-decretees_5497728_4355770.html

            let check' = check [DictionaryFr]

            it "ok sentence" $ do
                let l0 = "Journées du chat, des toilettes ou de la Résistance : comment et par qui sont-elles décrétées ?"
                rs <- check' [l0]
                rs `shouldBeList` [
                    ParagraphResult l0 []]

            it "miss sentence" $ do
                let l0 = "Journees du chat, des toiletes ou de la Résistance : comment et par qui sont elles décrétées ?"
                rs <- check' [l0]
                rs `shouldBeList` [
                    ParagraphResult l0 [
                        ResultMiss "Journees" 1 ["Journées","Ajournes","Séjournes","Journades","Séjourner"],
                        ResultMiss "toiletes" 23 ["toilettes","toilâtes"]]]

        describe "fr-fr" $ do
            -- https://www.lemonde.fr/les-decodeurs/article/2019/08/08/journee-du-chat-des-toilettes-ou-de-la-resistance-comment-et-par-qui-sont-elles-decretees_5497728_4355770.html

            let check' = check [DictionaryFrFr]

            it "ok sentence" $ do
                let l0 = "Journées du chat, des toilettes ou de la Résistance : comment et par qui sont-elles décrétées ?"
                rs <- check' [l0]
                rs `shouldBeList` [
                    ParagraphResult l0 []]

            it "miss sentence" $ do
                let l0 = "Journees du chat, des toiletes ou de la Résistance : comment et par qui sont elles décrétées ?"
                rs <- check' [l0]
                rs `shouldBeList` [
                    ParagraphResult l0 [
                        ResultMiss "Journees" 1 ["Journées","Ajournes","Séjournes","Journades","Séjourner"],
                        ResultMiss "toiletes" 23 ["toilettes","toilâtes"]]]

        describe "nl" $ do
            -- https://www.nu.nl/buitenland/5976175/bijna-zevenhonderd-immigranten-vs-uitgezet-na-politie-invallen.html

            let check' = check [DictionaryNl]

            it "ok sentence" $ do
                let l0 = "Ongeveer 680 illegale inwoners van de Amerikaanse staat Mississippi zijn woensdagavond na politie-invallen gearresteerd."
                rs <- check' [l0]
                rs `shouldBeList` [
                    ParagraphResult l0 []]

            it "miss sentence" $ do
                let l0 = "Ongeveer 680 ilegale inwoners von de Amerikaanse staat Mississippi zijn woensdagavond na politie-invallen gearresteert."
                rs <- check' [l0]
                rs `shouldBeList` [
                    ParagraphResult l0 [
                        ResultMiss "ilegale" 14 ["illegale","legale"],
                        ResultMiss "von" 31 ["vont","con","bon","vin","voj","on","vond","vonk","Avon","Yvon","ven","non","ion","ton","vos"],
                        ResultMiss "gearresteert." 107 ["gearresteerd"]]]

        describe "nl-nl" $ do
            -- https://www.nu.nl/buitenland/5976175/bijna-zevenhonderd-immigranten-vs-uitgezet-na-politie-invallen.html

            let check' = check [DictionaryNlNl]

            it "ok sentence" $ do
                let l0 = "Ongeveer 680 illegale inwoners van de Amerikaanse staat Mississippi zijn woensdagavond na politie-invallen gearresteerd."
                rs <- check' [l0]
                rs `shouldBeList` [
                    ParagraphResult l0 []]

            it "miss sentence" $ do
                let l0 = "Ongeveer 680 ilegale inwoners von de Amerikaanse staat Mississippi zijn woensdagavond na politie-invallen gearresteert."
                rs <- check' [l0]
                rs `shouldBeList` [
                    ParagraphResult l0 [
                        ResultMiss "ilegale" 14 ["illegale","legale"],
                        ResultMiss "von" 31 ["vont","con","bon","vin","voj","on","vond","vonk","Avon","Yvon","ven","non","ion","ton","vos"],
                        ResultMiss "gearresteert." 107 ["gearresteerd"]]]
