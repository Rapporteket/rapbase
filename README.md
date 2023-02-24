# rapbase <img src="man/figures/logo.svg" align="right" height="150" />

<!-- badges: start -->
[![Version](https://img.shields.io/github/v/release/rapporteket/rapbase?sort=semver)](https://github.com/rapporteket/rapbase/releases)
[![R build status](https://github.com/Rapporteket/rapbase/workflows/R-CMD-check/badge.svg)](https://github.com/Rapporteket/rapbase/actions)
[![codecov.io](https://codecov.io/gh/Rapporteket/rapbase/branch/master/graph/badge.svg)](https://app.codecov.io/gh/rapporteket/rapbase?branch=master)
[![CRAN status](https://www.r-pkg.org/badges/version/rapbase)](https://CRAN.R-project.org/package=rapbase)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-green.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![Doc](https://img.shields.io/badge/Doc--grey.svg)](https://rapporteket.github.io/rapbase/)
<!-- badges: end -->

*rapbase* is an R package holding common R functions for *Rapporteket* which is a reporting service for Norwegian medical quality registries. Each registry at *Rapporteket* will have their content structured as R packages that are all found under the [Rapporteket organization at GitHub](https://github.com/Rapporteket). Such packages are likely to depend on the *rapbase* package.

Top-level information regarding *Rapporteket* is [provided on a dedicated site (in Norwegian)](https://rapporteket.github.io/rapporteket/).

## Install
Install *rapbase* from CRAN:
```r
install.packages("rapbase")
```

The latest release of the package can be installed from GitHub:
```r
remotes::install_github("Rapporteket/rapbase@*release")
```

Or install the development version from GitHub with:
```r
remotes::install("Rapporteket/rapbase")

## Usage
Once the package is installed functions can be called from within R, *e.g.*:
```r
rapbase::halloRapporteket()
```
For a complete and updated view of the package documentation please consult the [*rapbase*-site](https://rapporteket.github.io/rapbase/index.html). Looking into [how others have implemented *rapbase* for various registries](https://github.com/Rapporteket) is also an excellent way of learning the use of *rapbase*. In the [rapRegTemplate package (in Norwegian)](https://github.com/Rapporteket/rapRegTemplate) that provides a Shiny application template to be used for making registries there is some boiler plate code to aid start up. Please feel free to also contact the maintainers and authors directly. 

## Issues
Please provide any comments (*e.g.* on proposed enhancements, shortcomings, errors) through the [issue tracker](https://github.com/Rapporteket/rapbase/issues).

## Contributing
If you want to make changes to this project please follow the [Contributing guide](https://rapporteket.github.io/rapbase/CONTRIBUTING.html). Proposed changes will be processed based on manual code reviews. Code that is accepted will be merged into the main branch and used for full scale TESTING and QA prior to making a release for PRODUCTION deployment.

For kick-starting, a development environment set-up is included and may be applied if [docker](https://docs.docker.com/get-docker/) and [docker-compose](https://docs.docker.com/compose/install/) is at hand. After cloning *rapbase* the development environment can be startet from a terminal at the local work copy root directory by:
```bash
docker-compose up
```
Navigate a browser to localhost on port 8787, log in to the [RStudio IDE](https://posit.co/products/open-source/rstudio/) and initiate the project by "clicking" the file *rapbase.Rproj* inside the *rapbase* directory. For development all suggested imports for the *rapbase* R package will be needed. To make sure these are installed use the R Console and run
```r
devtools::install_dev_deps()
```
After installing has finished all should be set to start change-build-test iterations :rocket:

## Ethics
Please note that the 'rapbase' project is released with a
[Contributor Code of Conduct](http://rapporteket.github.io/rapbase/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
