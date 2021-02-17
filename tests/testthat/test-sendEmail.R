# we do not have the ability to test email shipments, yet...
test_that("stopifnot...", {
  expect_error(sendEmail(
    conf = list(), to = "", subject = "", text = "",
    attFile = ""
  ))
})
