j <- '
{
    "AccessUnits": [
      {
        "UnitId": 100082,
        "ParentUnitId": 0,
        "HasDatabase": true,
        "ExternalId": 100082,
        "HealthUnitId": null,
        "Title": "Helse Bergen HF",
        "TitleWithPath": "Helse Bergen HF",
        "ValidFrom": null,
        "ValidTo": null,
        "ExtraData": null
      },
      {
        "UnitId": 102966,
        "ParentUnitId": 100082,
        "HasDatabase": true,
        "ExternalId": 102966,
        "HealthUnitId": null,
        "Title": "HB Hjerteavdelingen",
        "TitleWithPath": "Helse Bergen HF - HB Hjerteavdelingen",
        "ValidFrom": null,
        "ValidTo": null,
        "ExtraData": null
      },
      {
        "UnitId": 4001031,
        "ParentUnitId": 0,
        "HasDatabase": true,
        "ExternalId": 4001031,
        "HealthUnitId": null,
        "Title": "Oslo universitetssykehus HF",
        "TitleWithPath": "Oslo universitetssykehus HF",
        "ValidFrom": null,
        "ValidTo": null,
        "ExtraData": null
      },
      {
        "UnitId": 700328,
        "ParentUnitId": 4001031,
        "HasDatabase": true,
        "ExternalId": 700328,
        "HealthUnitId": null,
        "Title": "Hjerte-, lunge- og karklinikken",
        "TitleWithPath": "Oslo universitetssykehus HF - Hjerte-, lunge- og karklinikken",
        "ValidFrom": null,
        "ValidTo": null,
        "ExtraData": null
      },
      {
        "UnitId": 100320,
        "ParentUnitId": 0,
        "HasDatabase": true,
        "ExternalId": 100320,
        "HealthUnitId": null,
        "Title": "St. Olavs Hospital HF",
        "TitleWithPath": "St. Olavs Hospital HF",
        "ValidFrom": null,
        "ValidTo": null,
        "ExtraData": null
      },
      {
        "UnitId": 104284,
        "ParentUnitId": 100320,
        "HasDatabase": true,
        "ExternalId": 104284,
        "HealthUnitId": null,
        "Title": "Klinikk for Hjertemedisin",
        "TitleWithPath": "St. Olavs Hospital HF - Klinikk for Hjertemedisin",
        "ValidFrom": null,
        "ValidTo": null,
        "ExtraData": null
      },
      {
        "UnitId": 106944,
        "ParentUnitId": 0,
        "HasDatabase": true,
        "ExternalId": 106944,
        "HealthUnitId": null,
        "Title": "AHUS Gardermoen",
        "TitleWithPath": "AHUS Gardermoen",
        "ValidFrom": null,
        "ValidTo": null,
        "ExtraData": null
      },
      {
        "UnitId": 4214492,
        "ParentUnitId": 106944,
        "HasDatabase": true,
        "ExternalId": 4214492,
        "HealthUnitId": null,
        "Title": "Hjertemedisinsk avdeling",
        "TitleWithPath": "AHUS Gardermoen - Hjertemedisinsk avdeling",
        "ValidFrom": null,
        "ValidTo": null,
        "ExtraData": null
      }
    ]
  }
'

accesstree <- jsonlite::parse_json(j)

# Store it as an external data set in this package, please adjust path
# accordingly

# jsonlite::write_json(accesstree, "./inst/extdata/accesstree.json")
