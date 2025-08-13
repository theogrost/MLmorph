#' Load tabular data (xlsx, csv, or json)
#'
#' @param data_path Character scalar; path to a `.xlsx`, `.csv`, or `.json` file.
#'
#' @return A base \code{data.frame} with the imported data.
#' @export
#'
#' @examples
#' tmp_csv <- tempfile(fileext = ".csv")
#' utils::write.csv(data.frame(a = 1:2, b = c("x", "y")), tmp_csv, row.names = FALSE)
#' load_data(tmp_csv)
#'
#' tmp_json <- tempfile(fileext = ".json")
#' jsonlite::write_json(list(a = 1:2, b = c("x","y")), tmp_json, auto_unbox = TRUE)
#' load_data(tmp_json)
#'
#' tmp_xlsx <- tempfile(fileext = ".xlsx")
#' openxlsx::write.xlsx(data.frame(a = 1:2, b = c("x","y")), tmp_xlsx)
#' load_data(tmp_xlsx)
load_data <- function(data_path) {
    if (!is.character(data_path) || length(data_path) != 1L) {
        stop("`data_path` must be a character scalar.", call. = FALSE)
    }
    if (!file.exists(data_path)) {
        stop("File does not exist: ", data_path, call. = FALSE)
    }

    ftype <-  tools::file_ext(data_path) %>%
        tolower()
    if (ftype == "xlsx") {
        imported_data <- openxlsx::read.xlsx(data_path)
    } else if (ftype == "csv") {
        imported_data <- utils::read.csv(data_path, stringsAsFactors = FALSE)
    } else if (ftype == "json") {
        imported_data <- jsonlite::fromJSON(data_path,
                                            simplifyVector = TRUE,
                                            simplifyDataFrame = TRUE,
                                            flatten = TRUE
        )
        if (!is.data.frame(imported_data)) {
            imported_data <- as.data.frame(imported_data, stringsAsFactors = FALSE)
        }
    } else {
        stop("Unsupported file extension '",
             ftype, "'. Supported: xlsx, csv, json.", call. = FALSE)
    }

    if (!is.data.frame(imported_data)) {
        stop("Imported object is not a data.frame.", call. = FALSE)
    }
    imported_data
}
