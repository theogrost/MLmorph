#' Create random forests model
#'
#' @param data The data.frame with data used for the model estimation
#' @param dependent Predictor variables names
#' @param independent The predicted variable name
#' @param train_validate_split The proportion of data used for training, vs validation
#' @param ... Other arguments passed to the randomForest::randomForest() function
#'
#' @return Named list, with: model, variables_importance, model_performance_on_test
#' @export
#'
#' @examples
#' create_rf_model(data.frame(a = factorize_numeric_vector(runif(100, 10, 20), breaks_no = 3),
#' b = factorize_numeric_vector(runif(100, 1, 2), breaks_no = 5),
#' c = factorize_numeric_vector(runif(100, 4, 5), breaks_no = 2)))
create_rf_model <- function(
        data,
        dependent = colnames(data)[ncol(data)],
        independent = setdiff(colnames(data), dependent),
        train_validate_split = 0.8,
        ...) {
    rf_formula <- as.formula(paste(dependent, "~", paste(independent, collapse = "+")))
    train <- sample(seq_len(nrow(data)),
                    floor(nrow(data)*train_validate_split),
                    replace = FALSE)
    test <- seq_len(nrow(data))[-train]
    rf_model <- randomForest::randomForest(rf_formula,
                             data = data[train,],
                             importance = TRUE,
                             ...)
    variables_importance <- randomForest::importance(rf_model)
    test_predictions <- predict(rf_model, data[test,])
    model_performance_on_test <- caret::confusionMatrix(test_predictions,
                                                    data[test, dependent])
    return(list(model = rf_model,
                variables_importance = variables_importance,
                model_performance_on_test = model_performance_on_test))
}

# rfsm <- create_rf_model(ksdata2)
# print(randomForest::importance(rfsm), type = 1)
