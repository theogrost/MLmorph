#' Create a morphospace of predictor combinations with class probabilities
#'
#' @param the_data A data.frame used to derive unique values of predictors.
#' @param model A classification model fitted via a formula interface that
#'   supports \code{predict(model, newdata, type = "prob")} (e.g., \pkg{randomForest}).
#' @param shiny Logical; if \code{TRUE}, compute predictions in batches with
#'   \pkg{shiny} progress. Default \code{FALSE}.
#'
#' @return A list with components:
#' \itemize{
#'   \item \code{morphospace}: data frame with all predictor combinations,
#'         class label column (named as the dependent), \code{calculated} (probability),
#'         and \code{purely_simulated} flag.
#'   \item \code{dependent}: character scalar with the outcome name.
#'   \item \code{independent}: character vector of predictor names.
#'   \item \code{all_vars}: character vector \code{c(independent, dependent)}.
#'   \item \code{purely_simulated}: logical vector aligned with \code{morphospace}.
#' }
#' @export
#'
#' @examples
#' n  <- 60
#' y  <- factor(sample(letters[1:3], n, TRUE))
#' x1 <- factorize_numeric_vector(runif(n, 10, 20), breaks_no = 3)
#' x2 <- factorize_numeric_vector(runif(n,  1,  2), breaks_no = 3)
#' df <- data.frame(y, x1, x2)
#' fit <- create_rf_model(df, dependent = "y", ntree = 50)$model
#' ms  <- create_morphospace(df, fit)
#' names(ms)
create_morphospace <- function(the_data,
                               model,
                               shiny = FALSE) {

    if (!is.data.frame(the_data)) {
        stop("`the_data` must be a data.frame.", call. = FALSE)
    }
    term_labels <- tryCatch(attr(stats::terms(model), "term.labels"),
                            error = function(e) NULL)
    fvars <- tryCatch(all.vars(stats::formula(model)),
                      error = function(e) NULL)
    if (is.null(term_labels) || is.null(fvars)) {
        stop("`model` must carry terms/formula (fit via a formula interface).", call. = FALSE)
    }
    independent <- term_labels[term_labels %in% fvars]
    dependent <- setdiff(fvars, independent)
    if (length(dependent) != 1L) {
        stop("Detected 0 or multiple dependent variables; expected exactly one.", call. = FALSE)
    }
    miss <- setdiff(independent, names(the_data))
    if (length(miss)) {
        stop("Missing predictor columns in `the_data`: ", paste(miss, collapse = ", "), call. = FALSE)
    }

    value_lists <- lapply(the_data[independent], unique)
    ocols <- colnames(the_data)
    morphospace <- expand.grid(value_lists,
                               KEEP.OUT.ATTRS = FALSE,
                               stringsAsFactors = FALSE)
    if (shiny) {
        batch_size <- 10000
        n <- nrow(morphospace)
        batches <- ceiling(n / batch_size)
        preds <- vector("list", batches)
        shiny::withProgress(message = "Calculating morphospace", value = 0, {
            for (i in seq_len(batches)) {
                start <- (i - 1) * batch_size + 1
                end <- min(i * batch_size, n)
                preds[[i]] <- stats::predict(model,
                                             morphospace[start:end, ],
                                             type = "prob")
                shiny::incProgress(1 / batches)
            }
            prediction <- do.call(rbind, preds)
            morphospace <- cbind(morphospace, prediction)
            morphospace <- tidyr::pivot_longer(morphospace,
                                               cols = colnames(prediction),
                                               names_to = dependent,
                                               values_to = "calculated")
            morphospace[, dependent] <- factor(morphospace[[dependent]],
                                               levels = sort(unique(morphospace[[dependent]])))
            purely_simulated <- !(apply(morphospace[, c(dependent, independent)], 1, paste, collapse = "\r") %in%
                                      apply(the_data[, c(dependent, independent)], 1, paste, collapse = "\r"))
            morphospace$purely_simulated <- purely_simulated
        })
    } else {
        prediction <- stats::predict(model, morphospace, type = "prob")
        morphospace <- cbind(morphospace, prediction)
        morphospace <- tidyr::pivot_longer(morphospace,
                                           cols = colnames(prediction),
                                           names_to = dependent,
                                           values_to = "calculated")
        morphospace[, dependent] <- factor(morphospace[[dependent]],
                                           levels = sort(unique(morphospace[[dependent]])))
        purely_simulated <- !(apply(morphospace[, c(dependent, independent)], 1, paste, collapse = "\r") %in%
                                  apply(the_data[, c(dependent, independent)], 1, paste, collapse = "\r"))
        morphospace$purely_simulated <- purely_simulated
    }
    list(morphospace = morphospace,
         dependent = dependent,
         independent = independent,
         all_vars = c(independent, dependent),
         purely_simulated = purely_simulated)
}
