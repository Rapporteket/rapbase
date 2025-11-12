# Package index

## Configuration

Read configuration data

- [`getConfig()`](getConfig.md) : Get configuration for package, if any

## Database

Handle database connections and queries

- [`loadRegData()`](loadRegData.md)
  [`describeRegistryDb()`](loadRegData.md)
  [`nlinesRegistryDb()`](loadRegData.md) : Provider of data for
  registries at Rapporteket
- [`rapOpenDbConnection()`](rapOpenDbConnection.md) : Provide connection
  handle for data source at Rapporteket
- [`rapCloseDbConnection()`](rapCloseDbConnection.md) : Close down data
  connection handle
- [`exportApp()`](exportApp.md) : Shiny app with database export
  functionality

## Staging data

Handle staging data

- [`listStagingData()`](stagingData.md)
  [`mtimeStagingData()`](stagingData.md)
  [`saveStagingData()`](stagingData.md)
  [`loadStagingData()`](stagingData.md)
  [`deleteStagingData()`](stagingData.md)
  [`cleanStagingData()`](stagingData.md) : Staging data functions

## User information

Provides information on the logged in user in a shiny session

- [`userAttribute()`](userAttribute.md)
  [`getUserEmail()`](userAttribute.md)
  [`getUserFullName()`](userAttribute.md)
  [`getUserGroups()`](userAttribute.md)
  [`getUserName()`](userAttribute.md)
  [`getUserPhone()`](userAttribute.md)
  [`getUserReshId()`](userAttribute.md)
  [`getUserRole()`](userAttribute.md) : User attributes in container
  apps running behind shinyproxy
- [`userInfo()`](userInfo.md) : Provide user attributes based on
  environment context

## Logging

Handle logging

- [`loggerSetup()`](loggerSetup.md) : Settings for logging as json
- [`logShinyInputChanges()`](logShinyInputChanges.md) : Wrapper around
  logger::log_shiny_input_changes
- [`appLogger()`](logger.md) [`repLogger()`](logger.md)
  [`autLogger()`](logger.md) : Log user events in shiny applications at
  Rapporteket
- [`sanitizeLog()`](sanitizeLog.md) : Sanitize log entries that have
  reached end of life

## Automated reports

Handle automated reports

- [`readAutoReportData()`](readAutoReportData.md) : Read automated
  report metadata
- [`writeAutoReportData()`](writeAutoReportData.md) : Write automated
  report metadata
- [`runAutoReport()`](runAutoReport.md) : Run reports as defined in yaml
  config and ship content by email
- [`runBulletin()`](runBulletin.md) : Run bulletin auto reports
- [`createAutoReport()`](createAutoReport.md) : Create and add report to
  config
- [`deleteAutoReport()`](deleteAutoReport.md) : Delete existing report
  from config/db
- [`makeAutoReportTab()`](makeAutoReportTab.md) : Make table of
  automated reports
- [`makeRunDayOfYearSequence()`](makeRunDayOfYearSequence.md) : Make a
  sequence of day numbers from av given date and interval
- [`findNextRunDate()`](findNextRunDate.md) : Find next run date for
  automated reports
- [`filterAutoRep()`](filterAutoRep.md) : Filter auto report data
- [`autoReportUI()`](autoReport.md)
  [`autoReportOrgInput()`](autoReport.md)
  [`autoReportOrgServer()`](autoReport.md)
  [`autoReportFormatInput()`](autoReport.md)
  [`autoReportFormatServer()`](autoReport.md)
  [`autoReportInput()`](autoReport.md)
  [`autoReportServer()`](autoReport.md)
  [`autoReportServer2()`](autoReport.md) : Shiny modules and helper
  functions for registry auto reports
- [`.testAutoReport()`](dot-testAutoReport.md) : Simple test of
  automated report
- [`.getFun()`](dot-getFun.md) : Provide explicit reference to function
  for do.call

## Shiny

Helper functions to be used by shiny apps

- [`navbarWidgetInput()`](navbarWidget.md)
  [`navbarWidgetServer()`](navbarWidget.md)
  [`navbarWidgetServer2()`](navbarWidget.md)
  [`navbarWidgetApp()`](navbarWidget.md) : Shiny modules providing GUI
  and server logic for user info widget
- [`appNavbarUserWidget()`](appNavbarUserWidget.md) : Create widget for
  registry apps at Rapporteket
- [`exportGuideUI()`](exportGuide.md)
  [`exportGuideServer()`](exportGuide.md)
  [`exportGuideServer2()`](exportGuide.md)
  [`exportGuideApp()`](exportGuide.md) : Shiny modules providing the
  Export Guide
- [`statsGuideUI()`](statsGuide.md)
  [`statsGuideServer()`](statsGuide.md)
  [`statsGuideApp()`](statsGuide.md) : Shiny modules providing the Stats
  Guide
- [`exportUCInput()`](export.md) [`exportUCServer()`](export.md)
  [`exportUCServer2()`](export.md) [`exportUCApp()`](export.md)
  [`selectListPubkey()`](export.md) [`exportDb()`](export.md) : Shiny
  modules providing GUI and server logic for Export
- [`statsInput()`](stats.md) [`statsUI()`](stats.md)
  [`statsServer()`](stats.md) [`statsServer2()`](stats.md)
  [`statsApp()`](stats.md) [`logFormat()`](stats.md)
  [`logTimeFrame()`](stats.md) : Shiny modules and helper functions for
  registry usage reports
- [`theme()`](theme.md) : Theme of the app.
- [`title()`](title.md) : Title on the top left of the app, including
  the logo

## Miscellaneous

Various tools

- [`halloRapporteket()`](halloRapporteket.md) : Plain testing tool
- [`sendEmail()`](sendEmail.md) : Send email from Rapporteket
- [`howWeDealWithPersonalData()`](howWeDealWithPersonalData.md) : Render
  text in pop-up
- [`isRapContext()`](isRapContext.md) : Rapporteket context
- [`noOptOutOk()`](noOptOutOk.md) : Provide a no-opt-out ok message
- [`renderRmd()`](renderRmd.md) : Render documents from rmarkdown files
  at Rapporteket
- [`mst()`](makeStandardTable.md) : Make standard table for rmarkdown
  reports
- [`getGithub()`](getGithub.md) : Collect various data from the GitHub
  API

## Data

Data contained by the package

- [`appLog`](appLog.md) : App log test dataset.
- [`testdata`](testdata.md) : Test dataset.

## Meta

About the package

- [`rapbase`](rapbase.md) : rapbase: Base Functions and Resources for
  Rapporteket
