#' Create a random forest classification model
#'
#' @param data A \code{data.frame} containing predictors and the outcome.
#' @param dependent Character scalar; the name of the outcome (must be a factor for classification).
#'   Defaults to the last column of \code{data}.
#' @param independent Character vector; names of predictor variables.
#'   Defaults to all columns except \code{dependent}.
#' @param train_validate_split Numeric in (0, 1); proportion of rows used for training. Default is \code{0.8}.
#' @param shiny Logical; if \code{TRUE}, trains incrementally and reports progress via \pkg{shiny}. Default \code{FALSE}.
#' @param ... Additional arguments passed to \code{randomForest::randomForest()} (e.g., \code{ntree}).
#'
#' @return A named list with components:
#' \itemize{
#'   \item \code{model}: a \code{randomForest} object.
#'   \item \code{variables_importance}: matrix from \code{randomForest::importance()}.
#'   \item \code{model_performance_on_test}: a \code{caret::confusionMatrix} object on the validation set.
#' }
#' @export
#'
#' @examples
#' n <- 60
#' y  <- factor(sample(letters[1:3], n, TRUE))
#' x1 <- factorize_numeric_vector(runif(n, 10, 20), breaks_no = 3)
#' x2 <- factorize_numeric_vector(runif(n,  1,  2), breaks_no = 5)
#' df <- data.frame(y, x1, x2)
#' fit <- create_rf_model(df, dependent = "y", ntree = 50)
#' names(fit)
create_rf_model <- function(
        data,
        dependent = colnames(data)[ncol(data)],
        independent = setdiff(colnames(data), dependent),
        train_validate_split = 0.8,
        shiny = FALSE,
        ...) {

    if (!is.data.frame(data)) {
        stop("`data` must be a data.frame.", call. = FALSE)
    }
    if (!is.character(dependent) || length(dependent) != 1L || !dependent %in% names(data)) {
        stop("`dependent` must be the name of a column in `data`.", call. = FALSE)
    }
    if (!is.character(independent) || length(independent) < 1L) {
        stop("`independent` must be a non-empty character vector of column names.", call. = FALSE)
    }
    miss_ind <- setdiff(independent, names(data))
    if (length(miss_ind)) {
        stop("Unknown `independent` columns: ", paste(miss_ind, collapse = ", "), call. = FALSE)
    }
    if (dependent %in% independent) {
        stop("`dependent` cannot also appear in `independent`.", call. = FALSE)
    }
    if (!is.numeric(train_validate_split) || length(train_validate_split) != 1L ||
        is.na(train_validate_split) || train_validate_split <= 0 || train_validate_split >= 1) {
        stop("`train_validate_split` must be a single number in (0, 1).", call. = FALSE)
    }
    if (!is.factor(data[[dependent]])) {
        stop("`dependent` must be a factor for classification.", call. = FALSE)
    }
    if (length(stats::na.omit(unique(data[[dependent]]))) < 2L) {
        stop("`dependent` must have at least two classes.", call. = FALSE)
    }
    dots <- list(...)
    if (isTRUE(shiny)) {
        ntree <- dots$ntree
        if (is.null(ntree) || !is.numeric(ntree) || ntree %% 1 != 0 || ntree < 1) {
            stop("When `shiny = TRUE`, supply a positive integer `ntree`.", call. = FALSE)
        }
    }

    rf_formula <- stats::as.formula(paste(dependent, "~",
                                          paste(independent, collapse = "+")))
    train <- sample(seq_len(nrow(data)),
                    floor(nrow(data) * train_validate_split),
                    replace = FALSE)
    test <- seq_len(nrow(data))[-train]
    if (length(test) == 0L) {
        stop("Validation set is empty; decrease `train_validate_split`.", call. = FALSE)
    }
    if (length(unique(data[[dependent]][train])) < 2L) {
        stop("Training set has < 2 classes in `dependent`; stratify or adjust split.", call. = FALSE)
    }

    if (shiny) {
        ntree <- dots$ntree
        rf_list <- vector("list", ntree)
        oob_error <- numeric(ntree)
        shiny::withProgress(message = "Training Random Forest", value = 0, {
            for (i in seq_len(ntree)) {
                rf_list[[i]] <- randomForest::randomForest(
                    rf_formula, data = data[train, ], ntree = 1,
                    keep.forest = TRUE, importance = TRUE
                )
                rf_sub <- do.call(randomForest::combine, rf_list[1:i])
                pred <- stats::predict(rf_sub, data[test, ], type = "response")
                oob_error[i] <- mean(as.character(pred) != as.character(data[test, dependent]))
                shiny::incProgress(1 / ntree, detail = paste("Tree", i, "of", ntree))
            }
        })
        rf_model <- do.call(randomForest::combine, rf_list)
        rf_model$err.rate <- data.frame(error = oob_error)
    } else {
        rf_model <- randomForest::randomForest(
            rf_formula, data = data[train, ], importance = TRUE, ...
        )
    }
    variables_importance <- randomForest::importance(rf_model)
    test_predictions <- stats::predict(rf_model, data[test, ])
    model_performance_on_test <- caret::confusionMatrix(
        test_predictions, data[test, dependent]
    )

    list(
        model = rf_model,
        variables_importance = variables_importance,
        model_performance_on_test = model_performance_on_test
    )
}
