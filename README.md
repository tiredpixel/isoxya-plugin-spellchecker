# Isoxya plugin Spellchecker

Isoxya plugin Spellchecker provides spellchecking capabilities to entire websites, even if they have millions of pages, and supports 7 languages. It is a plugin for [Isoxya](https://www.isoxya.com/) web crawler.

https://hub.docker.com/r/isoxya/isoxya-plugin-spellchecker  
https://github.com/tiredpixel/isoxya-plugin-spellchecker  


## Languages

| Code     | Language | Variants               |
-----------|----------|------------------------|
| `en` `*` | English  | `gb` (BrE), `us` (AmE) |
| `cs`     | Czech    | `cz`                   |
| `de`     | German   | `de`                   |
| `es`     | Spanish  | `es` (European)        |
| `et`     | Estonian | `ee`                   |
| `fr`     | French   | `fr`                   |
| `nl`     | Dutch    | `nl`                   |

`*`: this is the default, if no language or variant is specified

Many other languages can be added easily, since both Hunspell and MySpell dictionaries are used. If it's available in the build OS, it can probably be added, with appropriate tests and extensions to the Isoxya engine interface.


## Example

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


## Installation

Compile and boot locally:

```sh
docker compose up
```

Images are also published using the `latest` tag (for development), and version-specific tags (for production). Do *not* use a `latest` tag in production!


## Licence

Copyright © [Nic Williams](https://www.tiredpixel.com/). It is free software, released under the BSD 3-Clause licence, and may be redistributed under the terms specified in `LICENSE`.
