context("Generate messages")

test_that("Function return one element atomic char vector", {
  story <- "Whole story so far."
  event <- "New part of story."
  
  expect_equal(length(MakeMessage(story, event)), 1)
  expect_true(is.atomic(MakeMessage(story, event)))
  expect_true(is.character(MakeMessage(story, event, newPara = TRUE)))
  expect_message(HalloRapporteket(), "Hallaisen")
})

test_that("Test function is operational", {
  expect_message(HalloRapporteket(), "Hallaisen")
})