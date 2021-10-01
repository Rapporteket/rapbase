test_that("guide UI returns a shiny tag object", {
  expect_equal(class(statsGuideUI("id")), "shiny.tag")
})

test_that("guide module server provides output", {
  shiny::testServer(statsGuideServer, args = list(registryName = "test"), {
    expect_equal(class(output$statsGuide), "list")
  })
})

test_that("guide test app returns an app object", {
  expect_equal(class(statsGuideApp()), "shiny.appobj")
})
