### Test environments
Due to the above changes these test have been (re-)run as part of the second resubmission:

* local Debian 9, R 3.6.1 (based on docker image from rocker/verse:3.6.1)
* Ubuntu 16.04 (on Travis CI): oldrel (R 3.5.3), rel (R 3.6.1) and devel (R 2019-08-05 r76918)
* Windows Server 2012 R2 x64 (on Appveyor): R 3.6.1 Patched (2019-08-04 r76915)
* win-builder: unstable (2019-07-05 r76784), release (R 3.6.1) and oldrelease (R 3.5.3)

## Test environments

### Win-builder

### R-hub

### GitHub Actions

* Ubuntu 20.04.4 LTS x86_64: R version 3.6.3 (2020-02-29)
* Ubuntu 18.04.6 LTS: R version 4.1.3 (2022-03-10)
* Ubuntu 20.04.4 LTS x86_64: R version 4.2.1 (2022-06-23)
* Ubuntu 20.04.4 LTS x86_64: R Under development (unstable) (2022-08-11 r82713)
* macOS Big Sur ... 10.16 x86_64: R version 4.2.1 (2022-06-23)
* Windows Server x64, x86_64: R version 4.2.1 (2022-06-23 ucrt)


## R CMD check results
There were no ERRORs and WARNINGs or NOTEs.
