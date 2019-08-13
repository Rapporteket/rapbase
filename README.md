# rapbase <img src="man/figures/favicon.ico" align="right" height="150" />

<!-- badges: start -->
[![Build Status](https://travis-ci.org/Rapporteket/rapbase.png)](https://travis-ci.org/Rapporteket/rapbase)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/areedv/rapbase?branch=master&svg=true)](https://ci.appveyor.com/project/areedv/rapbase)
[![codecov.io](https://codecov.io/github/Rapporteket/rapbase/rapbase.svg?branch=rel)](https://codecov.io/github/Rapporteket/rapbase?branch=rel)
[![Lifecycle: maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
<!-- badges: end -->

*rapbase* is an R package holding common R functions for *Rapporteket* which is a reporting service for Norwegian medical quality registries. Each registry at *Rapporteket* will have their content structured as R packages that are all found under the [Rapporteket organization at GitHub](https://github.com/Rapporteket). Such packages are likely to depend on the *rapbase* package.

Top-level information regarding *Rapporteket* is [provided on a dedicated site (in Norwegian)](https://rapporteket.github.io/rapporteket).

## Install
As of July 2019 the *rapbase* R package is available from GitHub and can be installed from the R command prompt:
```r
remotes::install("Rapporteket/rapbase")
```
Alternatively, the package source code can be [cloned from GitHub](https://github.com/Rapporteket/rapbase) and built locally. 

## Usage
Once the package is installed functions can be called from within R, *e.g.*:
```r
rapbase::HalloRapporteket()
```
For a complete and updated view of the package documentation please consult the [*rapbase*-site](https://rapporteket.github.io/rapbase/index.html). Looking into [how others have implemented *rapbase* for various registries](https://github.com/Rapporteket) is also an excellent way of learning the use of *rapbase*. In the [rapRegTemplate package (in Norwegian)](https://github.com/Rapporteket/rapRegTemplate) that provides a Shiny application template to be used for making registries there is some boiler plate code to aid start up. Please feel free to also contact the maintainers and authors directly. 

## Issues
Please provide any comments (*e.g.* on proposed enhancements, shortcomings, errors) through the [issue tracker](https://github.com/Rapporteket/rapbase/issues).

## Contributing
Contributors submit their code by forking from the 'rel' branch and opening of pull request. Development workflow must apply unit testing of the code. In addition to automated testing proposed changes and additions to the *rapbase* code will be accepted (or rejected) based on manual code reviews. Code that is accepted will be merged into the 'rel' branch, tagged and used for full scale TESTING and QA prior to PRODUCTION deployment.

## Ethics
Please note that the 'rapbase' project is released with a
[Contributor Code of Conduct](http://rapporteket.github.io/rapbase/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
