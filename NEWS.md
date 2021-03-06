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




