#' Loads data for the use of MLmorph
#'
#' @param data_path File path of the data
#'
#' @return A data.frame with imported data
#' @export
#'
#' @examples
#' xlsxFile <- system.file("extdata", "load_data_test.xlsx", package = "openxlsx")
#' df1 <- load_data(xlsxFile)
#'
#' csvFile <- system.file("extdata", "load_data_test.csv", package = "openxlsx")
#' df2 <- load_data(csvFile)
#'
#' jsonFile <- system.file("extdata", "load_data_test.json", package = "openxlsx")
#' df3 <- load_data(jsonFile)
load_data <- function(data_path) {
    ftype <- tools::file_ext(data_path)
    if(ftype == "xlsx") {
        imported_data <- openxlsx::read.xlsx(data_path)
    } else if(ftype == "csv") {
        imported_data <- utils::read.csv(data_path)
    } else if(ftype == "json") {
        imported_data <- jsonlite::read_json(data_path,
                                             simplifyVector = TRUE) %>%
            jsonlite::flatten()
    }
    return(imported_data)
}
