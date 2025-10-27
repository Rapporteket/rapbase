
# Test for the title function
test_that("title function returns a div with the correct structure", {
    result <- title("Test Title")

    # Check if the result is a shiny tag
    expect_s3_class(result, "shiny.tag")

    # Check if the div contains the title text
    expect_true(grepl("Test Title", as.character(result)))

    # Check if the div contains a svg element (the logo)
    expect_true(grepl("</svg>", as.character(result)))
})

# Test for the theme function
test_that("theme function returns a valid shinytheme object", {

    # Check if the theme name is correctly set
    expect_equal(theme(), "shinythemes/css/flatly.min.css")

    # Check if rubbish theme name returns an error
    expect_error(theme("qwerty"))
})
