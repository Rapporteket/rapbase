# Create widget for registry apps at Rapporteket

Provides a widget-like information and utility block to be applied to
all registry apps at Rapporteket. Contains the user name, organization
and logout/exit as hyperlinked text.

## Usage

``` r
appNavbarUserWidget(
  user = "Undefined person",
  organization = "Undefined organization",
  addUserInfo = FALSE,
  selectOrganization = FALSE,
  namespace = NULL
)
```

## Arguments

- user:

  String providing the name of the user

- organization:

  String providing the organization of the user

- addUserInfo:

  Logical defining whether a user data pop-up is to be part of the
  widget (TRUE) or not (FALSE, default)

- selectOrganization:

  Logical if organization can be selected.

- namespace:

  Character string providing the namespace to use, if any. Defaults is
  `NULL` in which case no namespace will be applied.

## Value

Ready made html script

## Details

Normally, user information will be be provided through the session
parameter and hence this will have to be provided from the server. The
"rendering" of this info must hence be done within a layout element at
the client such as a `tabPanel`. Selecting any one of them should be
fine... At the client, both `uiOutput` and `textOutput` will be fine
"rendering the information provided by the server.

Example of use in shiny (pseudo code):

    server <- function(input, output, session) {
      ...
      output$appUserName <- renderText(getUserName(session))
      output$appUserOrg <- renderText(getUserReshId(session))
      ...
    }

    ui <- tagList(
      navbarPage(
        ...,
        tabPanel(...,
        appNavbarUserWidget(user = uiOutput(appUserName),
        organization = textOutput(appUserOrg))
        ),
        ...
      )
    )

## Examples

``` r
appNavbarUserWidget()
#> <script>var header = $('.navbar> .container-fluid');
#> header.append('<div class="navbar-brand" style="float:right;vertical-align:super;font-size:65%">Undefined personUndefined organization</div>');
#> </script>
```
