# Send email from Rapporteket

This function can be used to send email from within R at Rapporteket. It
relies on (and must hence be provided) specific settings through local
configuration to work properly.

## Usage

``` r
sendEmail(conf, to, subject, text, attFile = NULL)
```

## Arguments

- conf:

  List containing (Rapporteket) config such as sender email address,
  SMTP server url and port number

- to:

  Character vector containing email addresses. May also contain full
  names like '`Jane Doe <janed@nowhere.com>`'

- subject:

  Character string providing email subject.

- text:

  Character string providing the plain email text in HTML format.

- attFile:

  Character string providing the full file path to an attachment.
  Default is NULL in which case no attachment is made

## Value

Invisible sending of email
