#' Turn numeric vector into ordered factor
#'
#' @param data_vector Vector with numeric data.
#' @param method Method to create breaks, "equal_bins" by default.
#' @param breaks_no Number of breaks to create, 5L by default.
#' @param custom_breaks Custom breaks to supply, as a named numeric vector.
#' @param custom_labels Custom labels to supply, as a named numeric vector.
#'
#' @return Factorized vector
#' @export
#'
#' @examples
#' factorize_numeric_vector(runif(50))
factorize_numeric_vector <- function(
        data_vector,
        method = c("equal_bins",
        "equal_distance",
        "custom_breaks"),
        breaks_no = 5,
        custom_breaks = NULL,
        custom_labels = NULL) {
    method <- method[1]
    breaks_no <- as.integer(breaks_no)
    if (!is.numeric(data_vector)) {
        stop("factorize_data error: data_vector must be a numeric vector.")
    }
    if(((is.null(custom_breaks))
        & method == "custom_breaks")) {
        stop('factorize_data error: If method = "custom_breaks",
             custom_breaks must be supplied.')
    }
    if ((!is.integer(breaks_no) & is.null(custom_breaks)) |
        (breaks_no <= 1 & is.null(custom_breaks))) {
        stop("factorize_data error: breaks_no must be an integer
             greater than 1.")
    }

    if(!is.null(custom_breaks)) {
        custom_breaks <- sort(custom_breaks)
        breaks <- custom_breaks
    } else {
        if(method == "equal_bins") {
            breaks <- unique(quantile(data_vector,
                                      probs = seq(0,
                                                  1,
                                                  length.out = breaks_no + 1),
                                      na.rm = TRUE))
        } else if(method == "equal_distance") {
            breaks <- seq(min(data_vector, na.rm = TRUE),
                          max(data_vector, na.rm = TRUE),
                          length.out = breaks_no + 1)
        }
        span <- min(diff(breaks))
        if (span >= 1) {
            digits <- 0
        } else {
            digits <- max(0, ceiling(abs(log10(span)))) + 1
        }
        breaks[1] <- floor(breaks[1] * 10^digits) / 10^digits
        breaks[length(breaks)] <- ceiling(breaks[length(breaks)] * 10^digits) /
            10^digits
        if (length(breaks) > 2) {
            intm <- 2:(length(breaks) - 1)
            breaks[intm] <- round(breaks[intm], digits = digits)
        }
    }
    if(!is.null(custom_labels)) {
        labs <- custom_labels
    } else {
        labs <- paste0("[", head(breaks, -1),
                         ", ",
                         tail(breaks, -1),
                         ")")
        labs[length(labs)] <- paste0("[",
                                         breaks[length(breaks) - 1],
                                         ", ",
                                         breaks[length(breaks)],
                                         "]")
        labs <- paste0(seq_along(labs), ". ", labs)
    }

    factorized_data <- cut(data_vector,
                           breaks = breaks,
                           labels = labs,
                           ordered_result = TRUE)
    return(factorized_data)
}


#' Turn binary vector into factor
#'
#' @param data_vector Vector with binary data.
#' @param custom_labels Custom labels vector: first for TRUE, second for FALSE.
#'
#' @return Factorized vector
#' @export
#'
#' @examples
#' factorize_binary_vector(c(TRUE, FALSE, TRUE))
factorize_binary_vector <- function(
        data_vector,
        custom_labels = NULL) {
    if(!is.null(custom_labels)) {
        labs <- custom_labels
    } else {
        labs <- c("1. True", "2. False")
    }
    data_vector <- factor(data_vector,
                          levels = c(TRUE, FALSE),
                          labels = labs)
    return(data_vector)
}

#' Turn character vector into factor
#'
#' @param data_vector Vector with character data.
#' @param custom_labels Custom labels named vector, names=levels, value=labels
#'
#' @return Factorized vector
#' @export
#'
#' @examples
#' factorize_character_vector(c("First", "Second", "Third"))
factorize_character_vector <- function(
        data_vector,
        custom_labels = NULL) {
    if(!is.null(custom_labels)) {
        data_vector <- factor(data_vector,
                              levels = names(custom_labels),
                              labels = custom_labels)
    } else {
        ll <- sort(unique(data_vector))
        labs <- paste0(seq_along(ll), ". ", ll)
        data_vector <- factor(data_vector,
                              levels = ll,
                              labels = labs)
    }
    return(data_vector)
}

factorize_identity <- function(
        data_vector) {
    ll <- sort(unique(data_vector))
    data_vector <- factor(data_vector,
                          levels = ll,
                          labels = ll)
    return(data_vector)
}

factorize_nicely_vector <- function(
        data_vector) {
    if(is.numeric(data_vector) & (sum(!duplicated(data_vector))>10)) {
        data_vector <- factorize_numeric_vector(data_vector)
    } else if(is.logical(data_vector)) {
        data_vector <- factorize_binary_vector(data_vector)
    } else {
        if(all(grepl("^[0-9]+\\.", data_vector))) {
            data_vector <- factorize_identity(data_vector)
        } else {
            data_vector <- factorize_character_vector(data_vector)
        }
    }
    return(data_vector)
}

factorize_nicely_dataframe <- function(
        data_frame) {
    for(i in seq_len(ncol(data_frame))) {
        data_frame[,i] <- factorize_nicely_vector(data_frame[,i])
    }
    return(data_frame)
}
