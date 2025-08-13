#' Zero-padded ordinal labels
#'
#' @param vec A vector; its length determines the padding width.
#' @return A character vector of zero-padded ordinals (e.g., "01", "02", …).
#' @keywords internal
get_label_numbers <- function(vec) {
    sprintf("%0*d", nchar(length(vec)), seq_along(vec))
}

#' Turn numeric vector into an ordered factor
#'
#' @param data_vector Numeric vector.
#' @param method Factorization rule: one of \code{"equal_bins"}, \code{"equal_distance"}, \code{"custom_breaks"}.
#' @param breaks_no Integer \eqn{\ge 2}; number of intervals when \code{method != "custom_breaks"}.
#' @param custom_breaks Optional numeric vector of cut points (strictly increasing) used when \code{method = "custom_breaks"}.
#' @param custom_labels Optional character vector of labels. If supplied, its length should equal \code{length(custom_breaks) - 1}.
#'
#' @return An ordered factor with interval labels.
#' @export
#'
#' @examples
#' factorize_numeric_vector(runif(10))
factorize_numeric_vector <- function(
        data_vector,
        method = c("equal_bins", "equal_distance", "custom_breaks"),
        breaks_no = 5,
        custom_breaks = NULL,
        custom_labels = NULL) {
    method <- method[1]
    if(breaks_no > sum(!duplicated(data_vector))) {
        breaks_no <- sum(!duplicated(data_vector))
    }
    breaks_no <- as.integer(breaks_no)
    if (!is.numeric(data_vector)) {
        stop("factorize_numeric_vector error: data_vector must be a numeric vector.")
    }
    if ((is.null(custom_breaks) & method == "custom_breaks")) {
        stop('factorize_numeric_vector error: If method = "custom_breaks",
             custom_breaks must be supplied.')
    }
    if (((length(custom_breaks) < 2) & method == "custom_breaks")) {
        stop('factorize_numeric_vector error: custom_breaks must be of length > 2.')
    }
    if ((!is.null(custom_breaks) & method == "custom_breaks" &
         !is.null(custom_labels)) &
        ((length(custom_breaks)-length(custom_labels))!=1)) {
        stop('factorize_numeric_vector error: length of custom_labels must be less 1 than length of custom_breaks.')
    }
    if (sum(!duplicated(data_vector))<2) {
        stop("factorize_numeric_vector error: at least two distinct values are required in `data_vector`.")
    }
    if ((!is.integer(breaks_no) & is.null(custom_breaks)) |
        (breaks_no <= 1 & is.null(custom_breaks))) {
        stop("factorize_numeric_vector error: breaks_no must be an integer
             greater than 1.")
    }
    if (!is.null(custom_breaks)) {
        custom_breaks <- sort(custom_breaks)
        breaks <- custom_breaks
    } else {
        if (method == "equal_bins") {
            breaks <- unique(stats::quantile(
                data_vector,
                probs = seq(0, 1, length.out = breaks_no + 1),
                na.rm = TRUE
            ))
        } else if (method == "equal_distance") {
            breaks <- seq(
                min(data_vector, na.rm = TRUE),
                max(data_vector, na.rm = TRUE),
                length.out = breaks_no + 1
            )
        }
        span <- min(diff(breaks))
        if (span >= 1) {
            digits <- 0
        } else {
            digits <- max(0, ceiling(abs(log10(span)))) + 1
        }
        breaks[1] <- floor(breaks[1] * 10^digits) / 10^digits
        breaks[length(breaks)] <- ceiling(breaks[length(breaks)] * 10^digits) / 10^digits
        if (length(breaks) > 2) {
            intm <- 2:(length(breaks) - 1)
            breaks[intm] <- round(breaks[intm], digits = digits)
        }
    }

    if (!is.null(custom_labels)) {
        labs <- custom_labels
    } else {
        labs <- paste0("[", utils::head(breaks, -1), ", ",
                       utils::tail(breaks, -1), ")")
        labs[length(labs)] <- paste0("[", breaks[length(breaks) - 1], ", ",
                                     breaks[length(breaks)], "]")
        labs <- paste0(get_label_numbers(labs), ". ", labs)
    }
    factorized_data <- cut(
        data_vector,
        breaks = breaks,
        labels = labs,
        ordered_result = TRUE,
        include.lowest = TRUE,
        right = TRUE
    )
    factorized_data
}

#' Turn binary vector into a factor
#'
#' @param data_vector Logical vector.
#' @param custom_labels Optional length-2 character vector: first for \code{TRUE}, second for \code{FALSE}.
#'
#' @return A factor with two levels in \code{TRUE}, \code{FALSE} order.
#' @export
#'
#' @examples
#' factorize_binary_vector(c(TRUE, FALSE, TRUE))
factorize_binary_vector <- function(
        data_vector,
        custom_labels = NULL
) {
    if (!is.logical(data_vector)) {
        stop("factorize_binary_vector error: data_vector must be a logical vector.", call. = FALSE)
    }
    if (!is.null(custom_labels)) {
        if (!is.character(custom_labels) || length(custom_labels) != 2L) {
            stop("factorize_binary_vector error: custom_labels must be a character vector of length 2.", call. = FALSE)
        }
        labs <- custom_labels
    } else {
        labs <- c("1. True", "2. False")
    }
    factor(data_vector, levels = c(TRUE, FALSE), labels = labs)
}

#' Turn character vector into a factor
#'
#' @param data_vector Character vector.
#' @param custom_labels Optional named character vector where names are original values and values are labels.
#'
#' @return A factor with labeled levels.
#' @export
#'
#' @examples
#' factorize_character_vector(c("A First", "B Second", "C Third"))
factorize_character_vector <- function(
        data_vector,
        custom_labels = NULL
) {
    if (!is.character(data_vector)) {
        stop("factorize_character_vector error: data_vector must be a character vector.", call. = FALSE)
    }
    if (!is.null(custom_labels)) {
        if (!is.character(custom_labels) || is.null(names(custom_labels))) {
            stop("factorize_character_vector error: custom_labels must be a named character vector.", call. = FALSE)
        }
        uniq <- sort(unique(data_vector))
        if (!all(uniq %in% names(custom_labels))) {
            stop("factorize_character_vector error: names(custom_labels) must cover all unique values.", call. = FALSE)
        }
        factor(data_vector, levels = names(custom_labels), labels = custom_labels)
    } else {
        ll <- sort(unique(data_vector))
        labs <- paste0(get_label_numbers(ll), ". ", ll)
        factor(data_vector, levels = ll, labels = labs)
    }
}

#' Identity factorization for numbered strings
#'
#' @param data_vector Character vector where values are already labeled (e.g., \code{"1. A"}).
#'
#' @return A factor with \code{levels == labels}.
#' @export
#'
#' @examples
#' factorize_identity(c("1. First", "2. Second", "3. Third"))
factorize_identity <- function(data_vector) {
    if (!is.character(data_vector)) {
        stop("factorize_identity error: data_vector must be a character vector.", call. = FALSE)
    }
    ll <- sort(unique(data_vector))
    factor(data_vector, levels = ll, labels = ll)
}

#' Heuristic factorization for a single vector
#'
#' @param data_vector A vector (numeric, logical, or character).
#'
#' @return A factor (ordered for numeric inputs with many distinct values).
#' @export
#'
#' @examples
#' factorize_nicely_vector(c("a", "b", "a"))
factorize_nicely_vector <- function(data_vector) {
    if (is.null(data_vector) || length(data_vector) == 0L) {
        stop("factorize_nicely_vector error: data_vector must be a non-empty vector.", call. = FALSE)
    }
    if (is.numeric(data_vector) && (sum(!duplicated(data_vector)) > 10L)) {
        factorize_numeric_vector(data_vector)
    } else if (is.logical(data_vector)) {
        factorize_binary_vector(data_vector)
    } else {
        # treat short numeric-like vectors as categories once coerced to character
        chr <- as.character(data_vector)
        if (all(grepl("^[0-9]+\\.", chr))) {
            factorize_identity(chr)
        } else {
            factorize_character_vector(chr)
        }
    }
}

#' Heuristic factorization for all columns of a data frame
#'
#' @param data_frame A data frame.
#' @return A data frame with all columns converted to factors.
#' @export
#'
#' @examples
#' df <- data.frame(x = runif(20), y = rep(c(TRUE, FALSE, TRUE, TRUE), 5))
#' factorize_nicely_dataframe(df)
factorize_nicely_dataframe <- function(data_frame) {
    if (!is.data.frame(data_frame)) {
        stop("factorize_nicely_dataframe error: data_frame must be a data.frame.", call. = FALSE)
    }
    for (i in seq_len(ncol(data_frame))) {
        data_frame[, i] <- factorize_nicely_vector(data_frame[, i])
    }
    data_frame
}
