with_mock_dir("gh_api_response", {
  test_that("error when dont't know what", {
    expect_error(getGithub("harrypotter", "wand"))
  })

  test_that("contributors are provided", {
    expect_equal(class(getGithub("contributors", "rapbase")), "character")
  })

  test_that("key can be provided", {
    expect_true(grepl("ssh-rsa", getGithub("keys", "areedv")))
  })
})