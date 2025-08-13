test_that("validation: data frame and columns", {
    expect_error(create_rf_model(1:3), "`data` must be a data.frame")
    df <- data.frame(y = factor(sample(c("a","b"), 10, TRUE)), x = 1:10)
    expect_error(create_rf_model(df, dependent = "nope"), "must be the name of a column")
    expect_error(create_rf_model(df, dependent = "y", independent = "nope"), "Unknown `independent`")
    expect_error(create_rf_model(df, dependent = "y", independent = c("x","y")), "cannot also appear")
})

test_that("validation: split, outcome type, and shiny type", {
    df <- data.frame(y = factor(sample(c("a","b"), 20, TRUE)), x = rnorm(20))
    expect_error(create_rf_model(df, dependent = "y", train_validate_split = 1), "in \\(0, 1\\)")
    df_bad <- data.frame(y = rep("a", 10), x = rnorm(10))
    expect_error(create_rf_model(transform(df_bad, y = factor(y)), dependent = "y"), "at least two classes")
    expect_error(create_rf_model(transform(df, y = as.character(y)), dependent = "y"), "must be a factor")
    expect_error(create_rf_model(df, dependent = "y", shiny = TRUE), "supply a positive integer `ntree`")
})
