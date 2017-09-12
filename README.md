[![Build Status](https://travis-ci.org/Rapporteket/rapbase.png)](https://travis-ci.org/Rapporteket/rapbase)

# Install
The current package can be fetched directly from your R session. If not already
present, first install the devtools-package from your R terminal:

```r
install.packages("devtools")
```

Then, install the rapbase package:

```r
devtools::install_github("Rapporteket/rapbase")
```

When installed at Rapporteket make sure clean-up is performed:

```r
devtools::install_github("Rapporteket/rapbase", args=c("--clean"))
```

This will add local configuration after the package has been installed


NOTE: Communicating through a proxy might cause the above install command to
fail. If so, try the following prior to the above install command:

```r  
library(httr)
set_config(
  use_proxy(url="18.91.12.23", port=8080, username="user",password="passwd")
  )
```

replacing the example parameter values with whatever applies for the
system the package is being installed on

# Develop
Contributors submit their code to the rel (release) branch which is subject to
testing at Rapporteket. Upon acceptance rel will me merged to the master
branch and tagged