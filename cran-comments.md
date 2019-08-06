## Second resubmission
Thank you for your comments. These are the changes in the second resubmission:

* Replaced \\dontrun{} by \\donttest{} in the following Rd-files: getShinyUserGroups.Rd, getShinyUserName.Rd, getShinyUserReshId.Rd, getShinyUserRole.Rd, getUserEmail.Rd, getUserFullName.Rd, getUserGroups.Rd, getUserName.Rd, getUserPhone.Rd, getUserReshId.Rd, getUserRole.Rd, runAutoReport.Rd, writeAutoReportData.Rd. In addition, for each \\donttest{} a comment on the reason for doing so was provided

* Ensured that functions (and their examples) do not write to the user's home filespace or anywhere else on the file system by altering the following files: AutoReportFuns.R

* Ensured that tests (in tests/testthat) do not write to the user's home filespace or anywhere else on the file system by altering the following files: test-auto-report-functions.R, test-config.R

* Due to dependencies of the above changes the following files were also altered: tests/testthat/test-userInfo.R (further isolating test session), R/AutoReportFuns.R (config only read within relevant condition)

* Corrected a typo in README.md

* Removed useless comments in tests/testthat/test-installGithubPackage.R and redundant data documentation (R/Helligdager2008til2022.R)

## Resubmission
This is a resubmission. Thanks for the comments (and quickly so) and accordingly I have:

* Added an url reference for Rapporteket in the Description field of DESCRIPTION. The url is to be regarded as permanent, however, its content is recently made since most existing documentation is Norwegian language only. A minor typo was also corrected in the Description field.

* Updated the reference in README.md to the CODE_OF_CONDUCT by a lasting url.

## Test environments
* local Debian 9, R 3.6.1 (based on docker image from rocker/verse:3.6.1)
* Ubuntu 16.04 (on Travis CI): oldrel (R 3.5.3), rel (R 3.6.1) and devel (R 2019-07-29 r76903)
* win-builder: release (R 3.6.1) and oldrelease (R 3.5.3)

## R CMD check results
There were no ERRORs and WARNINGs.

There was one NOTE:

* This is the first time this package is submitted to Cran