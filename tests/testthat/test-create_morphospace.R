test_that("create_morphospace returns expected structure and probabilities", {
    skip_if_not_installed("randomForest")
    skip_if_not_installed("caret")
    skip_if_not_installed("tidyr")

    set.seed(123)
    n  <- 80
    y  <- factor(sample(letters[1:3], n, TRUE))
    x1 <- factorize_numeric_vector(runif(n, 0, 1), breaks_no = 3)  # few unique bins
    x2 <- factorize_numeric_vector(runif(n, 0, 1), breaks_no = 2)
    df <- data.frame(y, x1, x2)

    rf <- create_rf_model(df, dependent = "y", ntree = 50)$model
    out <- create_morphospace(df, rf)

    expect_type(out, "list")
    expect_named(out, c("morphospace","dependent","independent","all_vars","purely_simulated"),
                 ignore.order = TRUE)

    ms <- out$morphospace
    expect_true(is.data.frame(ms))
    expect_true(all(c("calculated","purely_simulated", out$dependent, out$independent) %in% names(ms)))

    # Probabilities in [0,1] and sum to ~1 per predictor combination
    stopifnot(length(out$independent) >= 1)
    grp_cols <- out$independent
    sums <- aggregate(ms$calculated,
                      by = unname(ms[grp_cols]),
                      FUN = sum)
    expect_true(all(is.finite(ms$calculated)))
    expect_true(all(ms$calculated >= 0 & ms$calculated <= 1))
    expect_true(all(abs(sums$x - 1) < 1e-6))
})

test_that("observed rows appear in morphospace (purely_simulated = FALSE for seen combos)", {
    skip_if_not_installed("randomForest")
    skip_if_not_installed("caret")
    skip_if_not_installed("tidyr")

    set.seed(42)
    n  <- 50
    y  <- factor(sample(LETTERS[1:2], n, TRUE))
    x1 <- factorize_numeric_vector(runif(n), breaks_no = 3)
    x2 <- factorize_numeric_vector(runif(n), breaks_no = 3)
    df <- data.frame(y, x1, x2)

    rf <- create_rf_model(df, dependent = "y", ntree = 30)$model
    out <- create_morphospace(df, rf)

    paste_ms <- apply(out$morphospace[, c(out$dependent, out$independent)], 1, paste, collapse = "\r")
    paste_df <- apply(df[, c(out$dependent, out$independent)], 1, paste, collapse = "\r")

    # every observed triple is representable in morphospace
    expect_length(setdiff(unique(paste_df), unique(paste_ms)), 0L)

    # at least one morphospace row corresponds to observed data
    expect_true(any(!out$morphospace$purely_simulated))
})

test_that("validation errors are informative", {
    skip_if_not_installed("randomForest")
    skip_if_not_installed("caret")
    skip_if_not_installed("tidyr")

    df <- data.frame(y = factor(sample(c("a","b"), 20, TRUE)),
                     x = factorize_numeric_vector(runif(20), breaks_no = 2))
    rf <- create_rf_model(df, dependent = "y", ntree = 10)$model

    expect_error(create_morphospace(1:3, rf), "`the_data` must be a data.frame")
    # remove predictor column to trigger missing-column error
    expect_error(create_morphospace(df["y"], rf), "Missing predictor columns")
})

