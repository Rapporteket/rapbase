context("Messages")

test_that("Function return one element atomic char vector", {
  story <- "Whole story so far."
  message <- "New part of story."
  newPara <- TRUE
  
  expect_equal(length(MakeMessage(story, message, newPara)), 1)
  expect_true(is.atomic(MakeMessage(story, message, newPara)))
  expect_true(is.character(MakeMessage(story, message, newPara)))
})