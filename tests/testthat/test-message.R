context("Messages")

test_that("Function return one element atomic char vector", {
  story <- "Whole story so far."
  message <- "New part of story."
  
  expect_equal(length(MakeMessage(story = story, message = message)), 1)
  expect_true(is.atomic(MakeMessage(story = story, message = message)))
  expect_true(is.character(MakeMessage(story = story, message = message)))
})