# User attributes in container apps running behind shinyproxy

For apps running as containers particular environment variables must be
defined for an orderly handling of dynamic user privileges. This
function makes use of environmental variables defined by shinyproxy to
provide available privileges for the shiny application.

These are helper function for [`userInfo`](userInfo.md). When used
without a shiny session object calls to these functions is made without
any arguments. If redefining contexts is needed, please use
[`userInfo`](userInfo.md) instead.

## Usage

``` r
userAttribute(unit = NULL, map_orgname = NULL)

getUserEmail(...)

getUserFullName(...)

getUserGroups(...)

getUserName(...)

getUserPhone(...)

getUserReshId(...)

getUserRole(...)
```

## Arguments

- unit:

  Integer providing the look-up unit id. Default value is NULL in which
  case all privileges for `group` are returned.

- map_orgname:

  A data.frame containing two columns:

  UnitId

  :   unit ids

  orgname

  :   corresponding organization names

- ...:

  To keep the same interface as before. Not used.

## Value

Invisibly a list of user metadata and privileges:

- name:

  The username for whom the privileges apply.

- fullName:

  User full name

- phone:

  User phone number

- email:

  User email

- unit:

  Unit id under which the privileges are defined.

- org:

  Organization id for the user.

- role:

  Role of the user.

- orgName:

  Name of the organization as defined under the unit id.

String with user attribute

## Examples

``` r
# \donttest{
try(getUserEmail())
#> Error in userAttribute() : 
#>   Environmental variables FALK_EXTENDED_USER_RIGHTS and FALK_APP_ID must both be set!
# }
```
