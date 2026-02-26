# Log user events in shiny applications at Rapporteket

To be used for logging at application level (*i.e.* when a shiny session
is started) or at report level (*i.e.* each time a report is run).
Logging of single report events should be made from reactive
environments within the shiny server function or from within the
(report) functions used by the same reactive environments.

## Usage

``` r
appLogger(
  session,
  msg = "No message provided",
  .topcall = sys.call(-1),
  .topenv = parent.frame()
)

repLogger(
  session,
  msg = "No message provided",
  .topcall = sys.call(-1),
  .topenv = parent.frame()
)

autLogger(
  user,
  name,
  registryName,
  reshId,
  type,
  pkg,
  fun,
  param,
  msg = "No message provided",
  .topenv = parent.frame()
)
```

## Arguments

- session:

  Shiny session object to be used for getting user data. For testing and
  development purposes `session` can be replaced by
  [`list()`](https://rdrr.io/r/base/list.html) in which case various
  config options might be used to provide something sensible.

- msg:

  String providing a user defined message to be added to the log record.
  Default value is 'No message provided'.

- .topcall:

  Parent call (if any) calling this function. Used to provide the
  function call with arguments. Default value is `sys.call(-1)`.

- .topenv:

  Name of the parent environment calling this function. Used to provide
  package name (*i.e.* register) this function was called from. Default
  value is [`parent.frame()`](https://rdrr.io/r/base/sys.parent.html).

- user:

  String providing owner of an automated report. Its value should
  correspond to the actual user name as provided in a shiny session at
  Rapporteket. Only used for subscription reports that are run outside a
  shiny session.

- name:

  String providing full name of the report owner. Only used for
  automated reports that are run outside a shiny session.

- registryName:

  String providing registry name. Only used for automated reports that
  are run outside a shiny session.

- reshId:

  String providing the organization id of the (subscription) report
  author. Only used for automated reports that are run outside a shiny
  session.

- type:

  Character string defining the type of report. Only used for automated
  reports that are run outside a shiny session in which case its value
  will replace that of `.topcall`.

- pkg:

  Character string naming the package of the function that is to be
  logged. Only used for automated reports that are run outside a shiny
  session.

- fun:

  Character string naming the function that should be logged. Only used
  for automated reports that are run outside a shiny session.

- param:

  List of named function parameter. Only used for automated reports that
  are run outside a shiny session.

## Value

Returns nothing but calls a logging appender

## Details

The below fields will be appended to the log, in the following order:

1.  `time`: date-time as event is logged as
    `format(time, "%Y-%m-%d %H:%M:%S")`

2.  `user`: username as found in the shiny session object or as provided
    by function argument

3.  `name`: full name of user as found in the shiny session object

4.  `group`: users group membership as provided by the shiny session
    object. Normally, this will correspond to the registry the user
    belongs to

5.  `role`: users role as provided by the shiny session object. Its
    value will depend on whatever is delivered by the authorization
    provider, but for OpenQReg registries 'LU' (local user) and 'SC'
    (system coordinator) are typical values

6.  `resh_id`: the organization id of the current user as provided by
    the shiny session object, OR, when source of logging is auto
    reports, the organization ID of the data source used to make the
    report

7.  `environment`: environment from where the logger function was called
    (only provided by `repLogger()`)

8.  `call`: function (with arguments) from where the logger was called
    (only provided by `repLogger()`)

9.  message: an optional message defined as argument to the function

The `autLogger()` function is a special case to be used for automated
reports. Since such reports are run outside a reactive (shiny) context
shiny session data are not available to the logger. Hence, logging data
must be provided as arguments directly. As of rapbase version 1.12.0
logging of automated reports are already taken care of. Hence, this
function should not be applied per registry application.

## Note

Pseudo code of how `appLogger()` may be implemented:

    library(shiny)
    library(raplog)

    server <- function(input, output, session) {
      raplog::appLogger(session, msg = "Smerteregisteret: starting shiny app")
      ...
    }

Pseudo code on how `repLogger()` can be implemented as part of a
function in a reactive (shiny) context. First, this is an example of the
shiny server function with the (reactive) function `renderPlot()`
calling a function that provides a histogram:

    library(shiny)
    library(raplog)

    server <- function(input, output, session) {
      ...
      output$hist <- renderPlot({
        makeHist(data, var = input$var, bins = input$bins, session = session)
      })
      ...
    }

Then, logging is called within the function `makeHist()`:

    makeHist <- function(data, var, bins, ...) {

      if ("session" %in% names(list(...))) {
        raplog::repLogger(session = list(...)[["session"]],
                          msg = "Providing histogram")
      }
      ...
    }

## Examples

``` r
# \donttest{
# Depend on the environment variable R_RAP_CONFIG_PATH being set
try(appLogger(list()))
#> Error in userAttribute() : 
#>   Environmental variables FALK_EXTENDED_USER_RIGHTS and FALK_APP_ID must both be set!
# }

# \donttest{
# Depend on the environment variable R_RAP_CONFIG_PATH being set
try(repLogger(list()))
#> Error in userAttribute() : 
#>   Environmental variables FALK_EXTENDED_USER_RIGHTS and FALK_APP_ID must both be set!
# }

# \donttest{
# Depend on the environment variable R_RAP_CONFIG_PATH being set
try(autLogger(user = "ttester", registryName = "rapbase", reshId = "999999"))
#> Error in autLogger(user = "ttester", registryName = "rapbase", reshId = "999999") : 
#>   argument "name" is missing, with no default
# }
```
