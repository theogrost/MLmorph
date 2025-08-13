test_that("CSV loads into a data.frame", {
    tmp <- tempfile(fileext = ".csv")
    dat <- data.frame(a = 1:3, b = letters[1:3])
    utils::write.csv(dat, tmp, row.names = FALSE)

    out <- load_data(tmp)
    expect_s3_class(out, "data.frame")
    expect_equal(names(out), c("a","b"))
    expect_equal(nrow(out), 3L)
})

test_that("JSON loads into a data.frame", {
    tmp <- tempfile(fileext = ".json")
    jsonlite::write_json(list(a = 1:2, b = c("x","y")), tmp, auto_unbox = TRUE)
    out <- load_data(tmp)
    expect_s3_class(out, "data.frame")
    expect_true(all(c("a","b") %in% names(out)))
    expect_equal(nrow(out), 2L)
})

test_that("XLSX loads into a data.frame", {
    tmp <- tempfile(fileext = ".xlsx")
    openxlsx::write.xlsx(data.frame(a = 1:2, b = c("x","y")), tmp)
    out <- load_data(tmp)
    expect_s3_class(out, "data.frame")
    expect_equal(nrow(out), 2L)
})

test_that("informative errors for missing or unsupported files", {
    expect_error(load_data("this_file_should_not_exist_12345.csv"),
                 "does not exist")

    tmp_txt <- tempfile(fileext = ".txt")
    writeLines("a,b\n1,x", tmp_txt)
    expect_error(load_data(tmp_txt), "Unsupported file extension")
})
