
# Test for the title function
test_that("title function returns a div with the correct structure", {
    result <- regTitle("Test Title")

    # Check if the result is a shiny tag
    expect_s3_class(result, "shiny.tag")

    # Check if the div contains the title text
    expect_true(grepl("Test Title", as.character(result)))

    # Check if the div contains a svg element (the logo)
    expect_true(grepl("</svg>", as.character(result)))
})

# Test for the theme function
test_that("rapTheme function returns a valid shinytheme object", {

    # Check if the theme name is correctly set
    expect_true(grepl("flatly", as.character(rapTheme())))

    expect_false(grepl("flatly", as.character(rapTheme(theme = "darkly"))))

    expect_true(grepl("`bootstrap-version` = \"3\"", as.character(rapTheme())))
    expect_false(
      grepl("`bootstrap-version` = \"3\"", as.character(rapTheme(version = 5)))
    )

    # Check if rubbish theme name returns an error
    expect_error(rapTheme("qwerty"))
})
