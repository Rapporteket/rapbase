# Log report events in shiny applications at Rapporteket.

Updated version of [`repLogger()`](logger.md) to be used in reactive
contexts where user data is provided as reactive values. This is the
case for the when `user <- navbarWidgetServer2` which the way of
providing user data in shiny applications at Rapporteket. The use of the
original [`repLogger()`](logger.md) should be changed to `repLogger2` as
[`repLogger()`](logger.md) doesn't handle change of user roles during a
session, which can happen in the shiny applications at Rapporteket.

## Usage

``` r
repLogger2(
  user,
  msg = "No message provided",
  .topcall = sys.call(-1),
  .topenv = parent.frame()
)
```

## Arguments

- user:

  This needs to be a navbarWidgetServer2 object. Make sure to be in a
  reactive context when calling this function.

- msg:

  String providing a description of the event to be logged. Default
  value is 'No message provided'.

- .topcall:

  Parent call (if any) calling this function. Used to provide the
  function call with arguments. Default value is `sys.call(-1)`.

- .topenv:

  Name of the parent environment calling this function. Used to provide
  package name (*i.e.* register) this function was called from. Default
  value is [`parent.frame()`](https://rdrr.io/r/base/sys.parent.html).

## Note

If you are in the main shiny server, you can call `user` without passing
it as an argument to the server function, as `user` is available in the
main server environment. However, if you are in a module server
function, you need to pass `user` as an argument to the module server
function before you call `repLogger2(user)`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Inside a reactive context, for example within a
# downloadHandler() function, you can call:
rapbase::repLogger2(user, msg = "This is a test")
# make sure user exists inside server modules
# by passing it as an argument to the module server function:
module_server <- function(id, user) { ... }
} # }
```
