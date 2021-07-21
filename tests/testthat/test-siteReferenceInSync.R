test_that("all functions in pkg are listed _pkgdown.yml reference", {
  expect_warning(
    pkgdown::build_reference(pkg = find.package("rapbase"), preview = FALSE),
    regexp = NA
  )
})
