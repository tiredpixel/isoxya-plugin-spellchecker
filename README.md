# Isoxya web crawler plugin: Spellchecker

[Isoxya web crawler plugin: Spellchecker](https://github.com/isoxya/isoxya-plugin-spellchecker) is an open-source (BSD 3-Clause) processor plugin for [Isoxya](https://www.isoxya.com/) web crawler. This plugin provides spellchecking capabilities to entire websites, even if they have millions of pages. It can be used with [Isoxya web crawler Community Edition](https://github.com/isoxya/isoxya-ce) (Isoxya CE), a free and open-source (BSD 3-Clause) mini crawler, suitable for small crawls on a single computer.

The spellchecker backend is [Hunspell](https://hunspell.github.io/), the same spellchecker as is used in LibreOffice, Mozilla Firefox, Mozilla Thunderbird, Google Chrome, and various proprietary programs. Support for several languages is included out-the-box.

Since Isoxya supports both processor and streamer plugins using the Isoxya interfaces, this plugin is only one of many possibilities for processing human-language or other webpage data.

Also available is [Isoxya web crawler Pro Edition](https://www.isoxya.com/) (Isoxya PE), a commercial and closed-source distributed crawler, suitable for small, large, and humongous crawls on high-availability clusters of multiple computers. Both editions utilise flexible [plugins](https://www.isoxya.com/plugins/), allowing numerous programming languages to be used to extend the core engine via JSON [interfaces](https://docs.isoxya.com/#interfaces). Plugins written for Isoxya CE should typically scale to Isoxya PE with minimal or no changes. More details and licences are available [on request](mailto:en@isoxya.com).


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

Many other languages can be added easily, since both Hunspell and MySpell dictionaries are used. If it's available in the build OS, it can probably be added, with appropriate tests and extensions to the Isoxya engine interface.

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


## Contact

[en@isoxya.com](mailto:en@isoxya.com) · [isoxya.com](https://www.isoxya.com/)

[tp@tiredpixel.com](mailto:tp@tiredpixel.com) · [tiredpixel.com](https://www.tiredpixel.com/)

LinkedIn: [in/nic-williams](https://www.linkedin.com/in/nic-williams/) · GitHub: [tiredpixel](https://github.com/tiredpixel)


## Licence

Copyright © 2019-2021 [Nic Williams](https://www.tiredpixel.com/). It is free software, released under the BSD 3-Clause licence, and may be redistributed under the terms specified in `LICENSE`.
