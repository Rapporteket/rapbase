test_that("guide UI returns a shiny tag object", {
  expect_equal(class(exportGuideUI("id")), "shiny.tag")
})

test_that("guide module server provides output", {
  #local_edition(3)
  #rn <- shiny::reactiveVal()
  shiny::testServer(exportGuideServer, args = list(registryName = "test"), {
    #rn("test")
    expect_equal(class(output$exportGuide), "list")
  })
})

test_that("guide test app returns an app object", {
  expect_equal(class(exportGuideApp()), "shiny.appobj")
})
