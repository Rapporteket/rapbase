context("Prompting")

test_that("Function return one element atomic char vector", {
  story <- "Whole story so far."
  event <- "New part of story."
  newPara <- TRUE
  
  expect_equal(length(MakeMessage(story, event, newPara)), 1)
  expect_true(is.atomic(MakeMessage(story, event, newPara)))
  expect_true(is.character(MakeMessage(story, event, newPara)))
  
  expect_message(HalloRapporteket())
})