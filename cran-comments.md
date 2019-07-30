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