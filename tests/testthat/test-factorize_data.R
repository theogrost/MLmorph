test_that("numeric factorization works and is ordered", {
    x <- c(1, 2, 3, 4, 5, NA)
    out1 <- factorize_numeric_vector(x, method = "equal_bins", breaks_no = 2)
    out2 <- factorize_numeric_vector(x, method = "equal_distance", breaks_no = 2)

    expect_s3_class(out1, "ordered")
    expect_s3_class(out2, "ordered")
    expect_length(levels(out1), 2L)
    expect_true(is.na(out1[is.na(x)]))
})

test_that("numeric factorization validates inputs", {
    expect_error(factorize_numeric_vector("not numeric"), "must be a numeric vector")
    expect_error(factorize_numeric_vector(rep(1, 5)), "at least two distinct")
    expect_error(factorize_numeric_vector(1:5, breaks_no = 1), "greater than 1")
    expect_error(factorize_numeric_vector(1:20, method = "custom_breaks"), "must be supplied")
    expect_error(factorize_numeric_vector(1:20, method = "custom_breaks", custom_breaks = 1), "length > 2")
    expect_error(factorize_numeric_vector(1:5, method = "custom_breaks",
                                          custom_breaks = c(0, 1, 2),
                                          custom_labels = "x"),
                 "custom_labels must be less 1 than length of custom_breaks")
})

test_that("numeric factorization with custom breaks and labels works", {
    x <- c(0.1, 0.5, 0.9)
    br <- c(0, 0.6, 1)
    labs <- c("low", "high")
    out <- factorize_numeric_vector(x, method = "custom_breaks",
                                    custom_breaks = br, custom_labels = labs)
    expect_identical(levels(out), labs)
    expect_equal(as.character(out), c("low", "low", "high"))
})

test_that("binary factorization validates inputs and orders levels", {
    expect_error(factorize_binary_vector(c(1, 0)), "must be a logical vector")
    expect_error(factorize_binary_vector(c(TRUE, FALSE), custom_labels = "x"),
                 "length 2")
    x <- c(TRUE, FALSE, TRUE)
    out <- factorize_binary_vector(x)
    expect_s3_class(out, "factor")
    expect_identical(levels(out), c("1. True", "2. False"))
})

test_that("character factorization validates inputs and coverage", {
    expect_error(factorize_character_vector(letters[1:3] == "a"),
                 "must be a character vector")
    x <- c("b", "a", "b", "c")
    labs <- c(a = "A", b = "B", c = "C")
    out <- factorize_character_vector(x, labs)
    expect_identical(levels(out), c("A", "B", "C"))
    expect_equal(as.character(out)[1], "B")
    expect_error(factorize_character_vector(x, c(a = "A", b = "B")), "must cover")
})

test_that("identity factorization checks type", {
    expect_error(factorize_identity(1:3), "must be a character vector")
    x <- c("1. a", "2. b", "1. a")
    out <- factorize_identity(x)
    expect_identical(levels(out), sort(unique(x)))
    expect_equal(as.character(out)[1], "1. a")
})

test_that("heuristic vector factorization dispatches sensibly", {
    expect_s3_class(factorize_nicely_vector(1:20), "ordered")
    expect_s3_class(factorize_nicely_vector(c(TRUE, FALSE)), "factor")
    expect_s3_class(factorize_nicely_vector(c("a", "b", "a")), "factor")
    expect_s3_class(factorize_nicely_vector(c("1. a", "2. b")), "factor")
    expect_error(factorize_nicely_vector(NULL), "non-empty vector")
})

test_that("data frame factorization validates input and converts columns", {
    expect_error(factorize_nicely_dataframe(1:3), "must be a data.frame")
    df <- data.frame(num = 1:20, logi = c(TRUE, FALSE))
    out <- factorize_nicely_dataframe(df)
    expect_s3_class(out$num, "ordered")
    expect_s3_class(out$logi, "factor")
})
