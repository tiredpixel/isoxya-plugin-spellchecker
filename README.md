# Isoxya Pickax Spellchecker (Haskell)

Isoxya Pickax Spellchecker is an Isoxya Pickax providing spellchecking to SEO
and other internet-related data-processing activities. Using this in combination
with the proprietary Isoxya engine, it's possible to spellcheck entire websites,
even if they have millions of pages. The spellchecker backend is Hunspell, the
same spellchecker as is used in LibreOffice, Mozilla Firefox,
Mozilla Thunderbird, Google Chrome, and various proprietary programs.

Isoxya is a High-Performance Internet Data Processor and Web Crawler.
It is designed as a next-generation web crawler, scalable for large sites
(millions of pages), cost-effective for tiny sites (1+ pages), offering
flexible data processing using multi-industry plugins, delivering results via
data streaming to multiple storage backends. It is magicked via a REST API using
JSON, and is available now for private preview.


## Languages

CODE     | LANGUAGE | VARIANTS
---------|----------|---------
`en` `*` | English  | `gb` (BrE), `us` (AmE)
`cs`     | Czech    | `cz`
`de`     | German   | `de`
`es`     | Spanish  | `es` (European)
`et`     | Estonian | `ee`
`fr`     | French   | `fr`
`nl`     | Dutch    | `nl`

`*`: this is the default, if no language or variant is specified

Many other languages can be added easily, since both Hunspell and MySpell
dictionaries are used. If it's available in the build OS, it can probably be
added, with appropriate tests and extensions to the Isoxya engine interface.

### en: English

Variants:

- `en`: all variants together
- `en-gb`: GB (BrE)
- `en-us`: US (AmE)

Example:

```json
[
  {
    "paragraph": "GloBal heating is increesing droughts, soil erosion and wildfires while diminishing crop yields in the tropics and thawing permafrost near the Poles, says the report by the Intergovernmental Panel on Climate Change.",
    "results": [
      {
        "correct": false,
        "offset": 1,
        "status": "miss",
        "suggestions": [
          "Global",
          "Glob al",
          "Glob-al"
        ],
        "word": "GloBal"
      },
      {
        "correct": false,
        "offset": 19,
        "status": "miss",
        "suggestions": [
          "increasing",
          "screening",
          "resining",
          "cresting",
          "resisting"
        ],
        "word": "increesing"
      }
    ]
  }
]
```

### cs: Czech

Variants:

- `cs`: all variants together
- `cs-cz`: CZ

Example:

```json
[
  {
    "paragraph": "Česka republika si v rámci Evropské uni udržuje velmi dobrou kondici trhu práce.",
    "results": [
      {
        "correct": false,
        "offset": 37,
        "status": "miss",
        "suggestions": [
          "inu",
          "ni",
          "unie",
          "unii",
          "unií",
          "unci",
          "učni",
          "unik",
          "upni",
          "usni",
          "unit",
          "utni",
          "uhni",
          "ani",
          "oni"
        ],
        "word": "uni"
      }
    ]
  }
]
```

### de: German

Variants:

- `de`: all variants together
- `de-de`: DE

Example:

```json
[
  {
    "paragraph": "Die 140 millionen Euro teure Expedition Richtung Norden beginnt dann am 20. September. Sie fuhrt das Team von Norwegen entlang der sibirischen Küste Richtung Pol. Vor Ort werden Messungen im Meerwaßer, im Eis und in der Atmosphäre vorgenommen.",
    "results": [
      {
        "correct": false,
        "offset": 9,
        "status": "miss",
        "suggestions": [
          "Millionen",
          "millionen-",
          "-millionen",
          "Billionen"
        ],
        "word": "millionen"
      },
      {
        "correct": false,
        "offset": 192,
        "status": "miss",
        "suggestions": [
          "Meerwasser",
          "Meterware"
        ],
        "word": "Meerwaßer"
      }
    ]
  }
]
```

### es: Spanish

Variants:

- `es`: all variants together
- `es-es`: ES (European)

Example:

```json
[
  {
    "paragraph": "El planeta necesita un cambio del modelo alementario para combatir la crisis climatica",
    "results": [
      {
        "correct": false,
        "offset": 42,
        "status": "miss",
        "suggestions": [
          "alimentario",
          "suplementario",
          "complementario",
          "parlamentario",
          "argumentario"
        ],
        "word": "alementario"
      },
      {
        "correct": false,
        "offset": 78,
        "status": "miss",
        "suggestions": [
          "climática",
          "climatice",
          "climatiza",
          "climaticé"
        ],
        "word": "climatica"
      }
    ]
  }
]
```

### et: Estonian

Variants:

- `et`: all variants together
- `et-ee`: EE

Example:

```json
[
  {
    "paragraph": "\"Vedelkütusega raketimootori katsetuse ajal toiimus plahvatus ja seade vottis tuld,\" ütles ministeerium.",
    "results": [
      {
        "correct": false,
        "offset": 45,
        "status": "miss",
        "suggestions": [
          "toimus",
          "toismui",
          "toiaimus",
          "toieimus",
          "toisimus",
          "toiilmus",
          "toiuimus",
          "toioimus",
          "toimimus",
          "toiihmus",
          "toihimus",
          "toiäimus",
          "toiimbus",
          "toimunus",
          "toitumus"
        ],
        "word": "toiimus"
      },
      {
        "correct": false,
        "offset": 72,
        "status": "miss",
        "suggestions": [
          "vettis",
          "voltis",
          "nottis",
          "kottis",
          "võttis",
          "vtotis",
          "votits",
          "vttois",
          "tavotis"
        ],
        "word": "vottis"
      }
    ]
  }
]
```

### fr: French

Variants:

- `fr`: all variants together
- `fr-fr`: FR

Example:

```json
[
  {
    "paragraph": "Journees du chat, des toiletes ou de la Résistance : comment et par qui sont elles décrétées ?",
    "results": [
      {
        "correct": false,
        "offset": 1,
        "status": "miss",
        "suggestions": [
          "Journées",
          "Ajournes",
          "Séjournes",
          "Journades"
        ],
        "word": "Journees"
      },
      {
        "correct": false,
        "offset": 23,
        "status": "miss",
        "suggestions": [
          "toilettes",
          "toilâtes",
          "toile tes",
          "toile-tes"
        ],
        "word": "toiletes"
      }
    ]
  }
]
```

### nl: Dutch

Variants:

- `nl`: all variants together
- `nl-nl`: NL

Example:

```json
[
  {
    "paragraph": "Ongeveer 680 ilegale inwoners van de Amerikaanse staat Mississippi zijn woensdagavond na politie-invallen gearresteert.",
    "results": [
      {
        "correct": false,
        "offset": 14,
        "status": "miss",
        "suggestions": [
          "illegale",
          "legale"
        ],
        "word": "ilegale"
      },
      {
        "correct": false,
        "offset": 107,
        "status": "miss",
        "suggestions": [
          "gearresteerd"
        ],
        "word": "gearresteert."
      }
    ]
  }
]
```

## Blessing

May you find peace, and help others to do likewise.


## Contact

We've tried to make this document clear and accessible. If you have any feedback
about how we could improve it, or if there's any part of it you'd like to
discuss or clarify, we'd love to hear from you. Our contact details are:

Pavouk OÜ | <https://www.pavouk.tech/> | <mailto:en@pavouk.tech>


## Licence

Copyright © 2019 [Pavouk OÜ](https://www.pavouk.tech/).
It is free software, released under the BSD3 licence, and may be redistributed
under the terms specified in `LICENSE`.
