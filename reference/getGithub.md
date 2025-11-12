# Collect various data from the GitHub API

Collect various data from the GitHub API

## Usage

``` r
getGithub(what, value, .token = NULL)
```

## Arguments

- what:

  Character string defining the api endpoint. Currently one of
  `c("contributors", "members", "keys")`.

- value:

  Character string specifying what to collect from the given endpoint.
  For "contributors" this should be the name of the repository, for
  "members" value should be the team slug and for "keys" this should be
  a github user name.

- .token:

  Character string providing av valid token that will be used if the api
  call requires authentication. Listing of team members do require a
  token with the appropriate credentials.

## Value

Character vector with results from the GitHub api request
