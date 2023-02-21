# rapbase 1.24.0

In summary, registries at Rapporteket may now run as standalone container apps. Thus, shiny-server is no longer a requirement for app deployment. Below is a summary of what has been done.

* Extended handling of user attributes when behind an app proxy (spring boot/shinyproxy)
* Added a database backend for staging data
* Per app (registry) encryption of staging data (regardless of file or database backend)
* Added a vignette with a short description of staging data server side set-up

## Breaking changes
By introducing encryption, staging data will not work across this and previous versions. All existing staging data should therefore be removed when upgrading to this version of rapbase (or downgrading from this to any previous version). To remove all staging data delete all related files and directories. If staging data uses a database backend, the database itself should be dropped. There should be no need for further actions as both files and database will be recreated upon the next request for storing of staging data.  

# rapbase 1.23.1

* Fixed Fixed bug in log sanitizer function ([#130](https://github.com/Rapporteket/rapbase/pull/130))

# rapbase 1.23.0

* Reduced number of imports by replacing local function with functions from [sship](https://github.com/Rapporteket/sship) ([#127](https://github.com/Rapporteket/rapbase/pull/127))
* Added latex package microtype to to report template to for better printing and less hyphenation in pdf reports ([#123](https://github.com/Rapporteket/rapbase/pull/123)) 
* Added database as an optional log backend and function for (scheduled) clean of logs ([#112](https://github.com/Rapporteket/rapbase/pull/112))
* For logging to a database backend format set to json ([#120](https://github.com/Rapporteket/rapbase/pull/120))
* Reorganized and consolidated R files, removed deprecated and unused function and applied minor fixes to function flaws ([#121](https://github.com/Rapporteket/rapbase/pull/121))

# rapbase 1.22.0

* Improved first page graphics and hyperlinking in default latex template ([#104](https://github.com/Rapporteket/rapbase/pull/104))
* Moved from soft to hard deprecation for outdated functions which will now fail ([#105](https://github.com/Rapporteket/rapbase/pull/105))
* Added functions to handle staging of data used by registry apps ([#106](https://github.com/Rapporteket/rapbase/pull/106))
* Improved guide for export of data ([#108](https://github.com/Rapporteket/rapbase/pull/108))
* Reduced the number of R package imports, also by removing seldom or never used functionality ([#109](https://github.com/Rapporteket/rapbase/pull/109))

# rapbase 1.21.0

* Pre-set frequency of auto reports may now be set upon calling the auto report module server function
* Fixed bug for default first issue date when frequency set to year
* Fixed visual flaw in export GUI: download button does now show when no keys are found

# rapbase 1.20.2

* Fix short-term error in function finding next run date in auto reports
* As result of the above a new field "startDate" was added to auto report data with functions for upgrading existing data missing this field ([#99](https://github.com/Rapporteket/rapbase/pull/99)) and the start date is checked before reports are run ([#100](https://github.com/Rapporteket/rapbase/pull/100))
* R package sship now installed from Cran rather than GitHub ([#101](https://github.com/Rapporteket/rapbase/pull/101))

# rapbase 1.20.1

* Fix unit testing issue by also allowing "MockShinySession" as an attribute of the shiny session object

# rapbase 1.20.0

* Navbar user information widget provided as a shiny module

# rapbase 1.19.4

* Export available to team members only

# rapbase 1.19.3

* Added T1 font encoding to default LaTeX template for proper printing of symbols (_e.g._ > and <) in pdfs ([#94](https://github.com/Rapporteket/rapbase/pull/94))
* Replaced function none ascii with unicode characters

# rapbase 1.19.2

* Fixed sub optimal formatting of auto report yaml file, both converting old- and writing new data ([#93](https://github.com/Rapporteket/rapbase/pull/93))

# rapbase 1.19.1

* Fixed missing international language when producing pdf at Rapporteket
* Added option for more latex templates when rendering markdown
* Added a latex template for plain documents

# rapbase 1.19.0

* Added option for explicit registry repository name at github when collecting keys for export ([#91](https://github.com/Rapporteket/rapbase/pull/91)) 

# rapbase 1.18.0

* Added guide for use statistics report ([#90](https://github.com/Rapporteket/rapbase/pull/90))

# rapbase 1.17.1

* Fixed bug related to ambiguous class of arguments when rendering reports ([#86](https://github.com/Rapporteket/rapbase/pull/86)).
* Fixed bug in filtering stats per registry ([#87](https://github.com/Rapporteket/rapbase/pull/87)).
* Adjusted export with implicit encryption including fixing of inadequate reactivity in download handler ([#89](https://github.com/Rapporteket/rapbase/pull/88)) and applying [older R compatibility fix in sship](https://github.com/Rapporteket/sship/pull/24).

# rapbase 1.17.0

* Remove one (of too many) imports ([#80](https://github.com/Rapporteket/rapbase/pull/80)).
* Fixed and improved how organization and logging should be applied and understood for auto reports ([#84](https://github.com/Rapporteket/rapbase/pull/81)).
* Added option for setting eligibility upon calling of modules (autoReport, export and stats) ([#85](https://github.com/Rapporteket/rapbase/pull/82)).
* General guide for auto reports was updated making it more general and correcting minor errors.
* Fixed shortcoming dealing with organization for dispatchment and bulletins.

# rapbase 1.16.1

* Added missing reactive return from auto report server module ([#79](https://github.com/Rapporteket/rapbase/pull/79))

# rapbase 1.16.0

* New functions and shiny modules providing automated reports (subscriptions, dispatchments and bulletins) for registries ([#78](https://github.com/Rapporteket/rapbase/pull/78))

# rapbase 1.15.1

* Allow GitHub API calls by proxy ([#76](https://github.com/Rapporteket/rapbase/pull/76))
* Fixed failing error check for TEST and PROD contexts ([#77](https://github.com/Rapporteket/rapbase/pull/77))

# rapbase 1.15.0

* New functions and modules handling database exports ([#74](https://github.com/Rapporteket/rapbase/pull/74))
* Shiny modules for report use statistics that can be applied by registries ([#75](https://github.com/Rapporteket/rapbase/pull/75))
* Minor adjustment so that the presence of ids in auto report tables is optional upon table creation  


# rapbase 1.14.1

* New functions added to site doc

# rapbase 1.14.0 ([#70](https://github.com/Rapporteket/rapbase/pull/70))

* Added latex template for rendering RMarkdown through pandoc
* In addition, graphics and bookdown stuff also added as part of new template directory
* Added generic function for rendering RMarkdown for standalone and fragment use
* Added generic function for making sensible tables for both html and pdf output formats
* Fixed an error in general app info info ([#60](https://github.com/Rapporteket/rapbase/issues/60))

# rapbase 1.13.3 ([#68](https://github.com/Rapporteket/rapbase/pull/68))

* Minor adjustment to email subject (header field) format

# rapbase 1.13.2  ([#66](https://github.com/Rapporteket/rapbase/pull/66))

* Fixed email encoded subject (header field) split
* Properly parsing full names from session object containing unicode characters

# rapbase 1.13.1

* Included unit tests and coverage reporting depending on a test database during github actions (ci) 

# rapbase 1.13.0

* New function providing registry db metadata

# rapbase 1.12.2

* Auto report id added to table to aid needs during registry implementation

# rapbase 1.12.1

* Update contributor guidelines
* Re-establish build site as part of ci

# rapbase 1.12.0

* Relevant version info added to user pop-up #46
* Looks of user pop-up info changed #49
* Logging functions previously found in raplog now delivered by this package
* Built-in logging of automated reports #56
* New generic function for auto report GUI with option for name-id mapping #54
* Added new types of automated reports (dispatchment and bulletin) #47, #48
* Deprecated some functions for future clean-up #55
* Migrated ci from travis to gh actions

# rapbase 1.11.4

* clean-up code also deprecating some function for v1.12

# rapbase 1.11.3

* Remove rJava dependency

# rapbase 1.11.2

* Remove close option from user widget

# rapbase 1.11.1

* Fix for none-ascii characters in email subject (header)

# rapbase 1.11.0

* More options in config
* Some new functions, e.g. fireInTheHole() and howWeDealWithPersonalData()
* Extending report subscription options
* Some improvements in handling errors
* Improved readability of site

# rapbase 1.10.0

* Removed a lot of stuff that do not belong in this package
* Support for MySQL through RMariaDB, only
* Now under a GPL-3 license

# rapbase 1.9.0

* Added a `NEWS.md` file to track changes to the package.
* Changed license to MIT
* Updated README including Rapporteket logo and lifecycle badge
* Added Code of Conduct
* Added contribution guidelines
* New package site at https://rapporteket.github.io/rapbase/index.html
* Auto build site from Travis CI




