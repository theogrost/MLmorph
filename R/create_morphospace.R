create_morphospace <- function(the_data,
                                 model) {
    independent <- attr(terms(model), "term.labels")[
        attr(terms(model), "term.labels") %in%
            all.vars(formula(model))]
    dependent <- setdiff(all.vars(formula(model)), independent)
    value_lists <- lapply(the_data[independent], unique)
    ocols <- colnames(the_data)
    morphospace <- expand.grid(value_lists,
                               KEEP.OUT.ATTRS = FALSE,
                               stringsAsFactors = FALSE)
    prediction <- predict(model, morphospace, type = "prob")
    morphospace <- cbind(morphospace, prediction)
    morphospace <- tidyr::pivot_longer(morphospace,
                                       cols = colnames(prediction),
                                       names_to = dependent,
                                       values_to = "calculated")
    morphospace[,dependent] <- factor(morphospace[[dependent]],
                                      levels = sort(unique(
                                          morphospace[[dependent]])))
    purely_simulated <- !(apply(morphospace[,c(dependent, independent)]
                              , 1, paste, collapse = "\r") %in%
        apply(the_data[,c(dependent, independent)], 1, paste, collapse = "\r"))
    morphospace$purely_simulated <- purely_simulated
    return(list(morphospace = morphospace,
                dependent = dependent,
                independent = independent,
                all_vars = c(independent, dependent),
                purely_simulated = purely_simulated))
}
# morphospace_creation(ksdata2, rfsm$model)
