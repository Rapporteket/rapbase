test_that("guide UI returns a shiny tag object", {
  expect_equal(class(exportGuideUI("id")), "shiny.tag")
})

test_that("guide module server provides output", {
  shiny::testServer(exportGuideServer, args = list(dbName = "test"), {
    expect_equal(class(output$exportGuide), "list")
  })
})

test_that("guide module server 2 provides output", {
  shiny::testServer(exportGuideServer2, args = list(dbName = shiny::reactiveVal("test")), {
    expect_equal(class(output$exportGuide), "list")
  })
})


test_that("guide test app returns an app object", {
  expect_equal(class(exportGuideApp()), "shiny.appobj")
})
