test_that("MLmorph sets options and calls shiny::runApp with expected args", {
    skip_if_not_installed("shiny")

    app_dir <- system.file("MLmorph", package = "MLmorph")
    if (identical(app_dir, "")) {
        skip("App directory not available (package not installed with assets).")
    }

    res <- with_mocked_bindings(
        runApp = function(appDir, launch.browser, port, host) {
            expect_identical(appDir, app_dir)
            expect_identical(launch.browser, FALSE)
            expect_equal(port, 1234)
            expect_identical(host, getOption("shiny.host", "127.0.0.1"))
            list(
                shiny_max_req_option = getOption("shiny.maxRequestSize"),
                shiny_max_req_opt2  = getOption("shiny.maxRequestSize")
            )
        },
        .package = "shiny",
        {
            MLmorph(host = "127.0.0.1",
                    port = 1234,
                    launch.browser = FALSE,
                    maxUploadSize = 5L * 1024^2)
        }
    )

    expect_equal(res$shiny_max_req_option, 5L * 1024^2)
    expect_equal(res$shiny_max_req_opt2, 5L * 1024^2)
})

test_that("MLmorph returns runApp's return value (mocked)", {
    skip_if_not_installed("shiny")

    app_dir <- system.file("MLmorph", package = "MLmorph")
    if (identical(app_dir, "")) skip("App directory not available.")

    marker <- list(ok = TRUE)
    out <- with_mocked_bindings(
        runApp = function(appDir, launch.browser, port, host) marker,
        .package = "shiny",
        { MLmorph(launch.browser = FALSE, port = NULL) }
    )
    expect_identical(out, marker)
})

test_that("MLmorph_live calls runApp on inst path (mocked, internal helper)", {
    skip_on_cran()
    skip_if_not_installed("shiny")

    expected <- paste0(getwd(), "/inst/MLmorph")
    seen <- with_mocked_bindings(
        runApp = function(appDir, launch.browser, host) {
            expect_identical(launch.browser, TRUE)
            expect_identical(host, "127.0.0.1")
            appDir
        },
        .package = "shiny",
        { MLmorph_live() }
    )
    expect_identical(seen, expected)
})
