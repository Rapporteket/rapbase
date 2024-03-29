j <- '
{
    "AccessUnits": [
      {
        "UnitId": 1,
        "ParentUnitId": 0,
        "HasDatabase": true,
        "ExternalId": 100082,
        "HealthUnitId": 100082,
        "Title": "Helse Bergen HF - LU",
        "TitleWithPath": "Helse Bergen HF",
        "ValidFrom": null,
        "ValidTo": null,
        "ExtraData": "LU"
      },
      {
        "UnitId": 2,
        "ParentUnitId": 1,
        "HasDatabase": true,
        "ExternalId": 102966,
        "HealthUnitId": 102966,
        "Title": "HB Hjerteavdelingen - SC",
        "TitleWithPath": "Helse Bergen HF - HB Hjerteavdelingen",
        "ValidFrom": null,
        "ValidTo": null,
        "ExtraData": "SC"
      },
      {
        "UnitId": 3,
        "ParentUnitId": 0,
        "HasDatabase": true,
        "ExternalId": 4001031,
        "HealthUnitId": 4001031,
        "Title": "Oslo universitetssykehus HF - SC",
        "TitleWithPath": "Oslo universitetssykehus HF",
        "ValidFrom": null,
        "ValidTo": null,
        "ExtraData": "SC"
      },
      {
        "UnitId": 4,
        "ParentUnitId": 3,
        "HasDatabase": true,
        "ExternalId": 700328,
        "HealthUnitId": 700328,
        "Title": "Hjerte-, lunge- og karklinikken - LU",
        "TitleWithPath": "Oslo universitetssykehus HF - Hjerte-, lunge- og karklinikken",
        "ValidFrom": null,
        "ValidTo": null,
        "ExtraData": "LU"
      },
      {
        "UnitId": 5,
        "ParentUnitId": 0,
        "HasDatabase": true,
        "ExternalId": 100320,
        "HealthUnitId": 100320,
        "Title": "St. Olavs Hospital HF - SC",
        "TitleWithPath": "St. Olavs Hospital HF",
        "ValidFrom": null,
        "ValidTo": null,
        "ExtraData": "SC"
      },
      {
        "UnitId": 6,
        "ParentUnitId": 5,
        "HasDatabase": true,
        "ExternalId": 104284,
        "HealthUnitId": 104284,
        "Title": "Klinikk for Hjertemedisin - LU",
        "TitleWithPath": "St. Olavs Hospital HF - Klinikk for Hjertemedisin",
        "ValidFrom": null,
        "ValidTo": null,
        "ExtraData": "LU"
      },
      {
        "UnitId": 7,
        "ParentUnitId": 0,
        "HasDatabase": true,
        "ExternalId": 106944,
        "HealthUnitId": 106944,
        "Title": "AHUS Gardermoen - SC",
        "TitleWithPath": "AHUS Gardermoen",
        "ValidFrom": null,
        "ValidTo": null,
        "ExtraData": "SC"
      },
      {
        "UnitId": 8,
        "ParentUnitId": 7,
        "HasDatabase": true,
        "ExternalId": 4214492,
        "HealthUnitId": 4214492,
        "Title": "Hjertemedisinsk avdeling - LU",
        "TitleWithPath": "AHUS Gardermoen - Hjertemedisinsk avdeling",
        "ValidFrom": null,
        "ValidTo": null,
        "ExtraData": "LU"
      }
    ]
  }
'

accesstree <- jsonlite::parse_json(j)

# Store it as an external data set in this package, please adjust path
# accordingly

# jsonlite::write_json(accesstree, "./inst/extdata/accesstree.json")
