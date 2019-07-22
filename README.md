# rapbase <img src="man/figures/favicon.ico" align="right" height="150" />

<!-- badges: start -->
[![Build Status](https://travis-ci.org/Rapporteket/rapbase.png)](https://travis-ci.org/Rapporteket/rapbase)
[![codecov.io](https://codecov.io/github/Rapporteket/rapbase/rapbase.svg?branch=rel)](https://codecov.io/github/Rapporteket/rapbase?branch=rel)
[![Lifecycle: maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
<!-- badges: end -->

is an R package holding common R functions for *Rapporteket* which is a reporting service for Norwegian medical quality registries. Each registry at *Rapporteket* will have their content structured as R packages that are all found under the [Rapporteket organization at GitHub](https://github.com/Rapporteket). Such packages are likely to depend on the *rapbase* package. Please feel free to [learn more about the *rapbase* package](https://rapporteket.github.io/rapbase/index.html).

Top-level information regarding *Rapporteket* is [provided on a dedicated site (in Norwegian)](https://rapporteket.github.io/rapporteket).

## Install
As of july 2019 the *rapbase* R package is available from GitHub and cab be installed from the R command propt:
```r
remotes::install("Rapporteket/rapbase")
```
Alternatively, the package source code can be [cloned from GitHub](https://github.com/Rapporteket/rapbase) and built locally. 

## Usage
Once the package is installed functions can be called such as:
```r
rapbase::HalloRapporteket()
```
For more information please consult the [*rapbase*-site](https://rapporteket.github.io/rapbase/index.html). Looking into [how others have implemented *rapbase* for various registries](https://github.com/Rapporteket) is also an excellent source of information. Please feel free also to contact the maintainers and authors directly. 

## Issues
Please provide any comments (*e.g.* on proposed enhancements, shortcomings, errors) through the [issue tracker](https://github.com/Rapporteket/rapbase/issues).

## Develop
Contributors submit their code by forking from the 'rel' branch and subseqent pull request. Development workflow must also include the process of unit testing of the code. In addition to automated testing proposed changes and additions to the *rapbase* code will be accepted (or rejected) based on manual code reviews. Code that is accepted will be merged into the 'rel' branch, tagged and used for full scale TESTING and QA prior to PRODUCTION deployment.
## Ethics
Please note that the 'rapbase' project is released with a
[Contributor Code of Conduct](CODE_OF_CONDUCT.md).
By contributing to this project, you agree to abide by its terms.