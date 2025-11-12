# Provide user attributes based on environment context

Extracts elements from either config, url (shiny), shiny session or
environmental variables relevant for user data such as name, group, role
and org id (*e.g.* resh id). Source of info is based on environment
context and can be controlled by altering the default settings for which
contexts that will apply for the various sources of user data. This
function will normally be used via its helper functions (see below).

## Usage

``` r
userInfo(entity)
```

## Arguments

- entity:

  String defining the element to return. Currently, one of 'user',
  groups', 'resh_id', 'role', 'email', 'full_name' or 'phone'.

## Value

String of single user data element

## See also

[`getUserName`](userAttribute.md), [`getUserGroups`](userAttribute.md),
[`getUserReshId`](userAttribute.md), [`getUserRole`](userAttribute.md)
