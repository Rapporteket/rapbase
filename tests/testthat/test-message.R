context("Messages")

story <- "Whole story so far."
message <- "New part of story."

test_that("Function return one element char vector", {
  testMessage <- function() MakeMessage(story = story, message = message)
  expect_equal(length(testMessage), 1)
  expect_equal(is.atomic(testMessage), TRUE)
  expect_equal(is.character(testMessage), TRUE)
})