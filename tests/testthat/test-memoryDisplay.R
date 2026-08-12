# Test for memoryDisplay behavior in a Shiny server
test_that("memoryDisplay renders memory text in server output", {
  server <- function(input, output, session) {
    output$memory <- memoryDisplay(session)
  }

  shiny::testServer(server, {
    session$flushReact()
    expect_match(output$memory, "^Memory: [0-9]+(\\.[0-9])? MB$")
  })
})

# Test for memoryTitle helper structure
test_that("memoryTitle adds app title and memory output", {
  title <- memoryTitle("Test App")

  expect_true(grepl("id=\"memory\"", as.character(title)))
  expect_true(grepl("Test App", as.character(title)))
})
