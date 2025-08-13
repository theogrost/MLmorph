library(shiny)
library(shinyjs)
library(shinyFiles)
library(bslib)
library(thematic)
library(htmltools)
library(plotly)
library(reactable)
library(ggplot2)
library(MLmorph)

# --- Global Reactive-Like Storage ---
imported_data <- NULL
factorized_data <- NULL
rf_model <- NULL
full_morphospace <- NULL
restricted_morphospace <- NULL
restrictions <- list(exclude_simulated = FALSE)
dict <- NULL

# --- Utilities ---
render_factorization_controls <- function(data_frame) {
    colnames <- names(data_frame)
    ui_list <- lapply(seq_along(colnames), function(i) {
        colname <- colnames[i]
        datatype <- class(data_frame[[colname]])[1]

        tagList(
            fluidRow(
                column(
                    width = 6,
                    tags$h4(sprintf("%s (%s)", colname, datatype))
                ),
                column(
                    width = 6,
                    selectInput(
                        inputId = paste0("fact_method_", i),
                        label = "Factorization method",
                        choices = c(
                            "nicely", "numeric_bins", "numeric_distance",
                            "custom_breaks", "binary", "character", "identity", "drop"
                        ),
                        selected = "nicely",
                        width = "100%"
                    )
                )
            ),
            conditionalPanel(
                condition = sprintf("input.fact_method_%s == 'numeric_bins' || input.fact_method_%s == 'numeric_distance'", i, i),
                fluidRow(
                    column(
                        width = 12,
                        sliderInput(
                            inputId = paste0("breaks_no_", i),
                            label = "Number of breaks",
                            min = 2, max = 10, value = 5, step = 1,
                            width = "100%"
                        )
                    )
                )
            ),
            conditionalPanel(
                condition = sprintf("input.fact_method_%s == 'custom_breaks'", i),
                fluidRow(
                    column(
                        width = 6,
                        textInput(
                            inputId = paste0("custom_breaks_", i),
                            label = "Breaks (comma-separated)",
                            value = "",
                            width = "100%"
                        )
                    ),
                    column(
                        width = 6,
                        textInput(
                            inputId = paste0("custom_labels_", i),
                            label = "Labels (comma-separated, optional)",
                            value = "",
                            width = "100%"
                        )
                    )
                )
            )
        )
    })
    return(ui_list)
}

make_color <- function(x,
                       min_val = min(full_morphospace$morphospace$calculated),
                       max_val = max(full_morphospace$morphospace$calculated)) {
    if(is.na(x)) {
        return("gray50")
    }
    scaled <- pmin(pmax((x - min_val) / (max_val - min_val), 0), 1)
    rgb(colorRamp(c("#42B4EB", "#FFFFFF", "#f89b29"))(scaled), maxColorValue = 255) # #FFFFFF
}

make_html_table_from_list <- function(vec_list) {
    max_len <- max(lengths(vec_list))
    rows <- lapply(seq_len(max_len), function(i) {
        tags$tr(lapply(vec_list, function(col) {
            value <- if (i <= length(col)) col[i] else NA
            td <- tags$td(if (!is.na(value)) value else "")
            if (!is.na(value)) {
                td$attribs$style <- "background-color: #f89b29;"
                td$attribs$class <- "non-empty"
            }
            td
        }))
    })
    tags$table(
        tags$thead(tags$tr(lapply(names(vec_list), tags$th))),
        tags$tbody(rows)
    )
}

make_morphmodel <- function() {
    df <- restricted_morphospace[, full_morphospace$all_vars]
    vec_list <- lapply(df, function(col) as.character(levels(col)))
    vec_list$calculated <- NULL
    max_len <- max(lengths(vec_list))
    dict <<- matrix("", nrow = max_len, ncol = ncol(df)) %>%
        magrittr::set_colnames(colnames(df))
    rows <- lapply(seq_len(max_len), function(i) {
        tags$tr(lapply(seq_along(vec_list), function(j) {
            val <- vec_list[[j]][i]
            dict[i,j] <<- val
            if (is.na(val)) return(tags$td(""))
            matches <- restricted_morphospace$calculated[df[[j]] == val]
            color <- make_color(if(length(matches) > 0)
                max(matches, na.rm = TRUE) else NA)
            td <- tags$td(val)
            td$attribs$style <- sprintf(
                "background-color: %s;", color)
            td$attribs$class <- paste(
                "non-empty", if(!is.null(restrictions[[as.character(j)]]) &&
                                restrictions[[as.character(j)]] == i) "selected" else NULL)
            td$attribs$onclick <- sprintf(
                'Shiny.setInputValue("toggle_restrictions", "col:%s;val:%s", {priority: "event"})', j, i)
            td
        }))
    })
    tbl <- tags$table(
        tags$thead(tags$tr(lapply(names(vec_list), tags$th))),
        tags$tbody(rows)
    )
    info <- tags$p(sprintf(
        "Max belief: %.2f%%",
        max(restricted_morphospace$calculated) * 100))
    tagList(tbl, info)
}

update_morphospace <- function() {
    restricted_morphospace <<- if (restrictions$exclude_simulated) {
        dplyr::filter(full_morphospace$morphospace, !purely_simulated)
    } else {
        full_morphospace$morphospace
    }
    if (length(restrictions) > 1) {
        other <- restrictions[-which(names(restrictions) == "exclude_simulated")]
        for (resname in names(other)) {
            colname <- colnames(dict)[as.numeric(resname)]
            val <- dict[as.numeric(other[[resname]]), colname]
            # colname <- colnames(restricted_morphospace)[as.numeric(resname)]
            # val <- sort(levels(restricted_morphospace[[colname]]))[other[[resname]]]
            restricted_morphospace <<- dplyr::filter(
                restricted_morphospace, .data[[colname]] == val)
        }
    }
}

# --- Server ---
server <- function(input, output) {
    shinyjs::disable(selector = 'a[data-value="> Configure data"]')
    shinyjs::disable(selector = 'a[data-value="> Create ML model"]')
    shinyjs::disable(selector = 'a[data-value="> Create morphospace"]')
    shinyjs::disable(selector = 'a[data-value="> Visualize MLmorph model"]')
    shinyjs::disable(selector = 'a[data-value="> Export MLmorph model"]')
    observeEvent(input$file_import, {
        req(input$file_import)
        imported_data <<- MLmorph::load_data(input$file_import$datapath)
        output$import_results <- renderPrint(HTML(
            sprintf(
                "<p>Imported %d records of %d variables.</p><p>Now, configure data.</p>",
                nrow(imported_data), ncol(imported_data)
            )
        ))
        output$factorization_controls <- renderUI({
            render_factorization_controls(imported_data)
        })
        shinyjs::enable(selector = 'a[data-value="> Configure data"]')
    })

    observeEvent(input$generate_preview, {
        factorized_data <<- factorize_nicely_dataframe(imported_data)
        lev_list <- lapply(factorized_data, levels)
        lev_list <- setNames(lev_list, colnames(factorized_data))
        output$configure_preview <- renderUI(
            make_html_table_from_list(lev_list))

        output$rf_controls <- renderUI({
            last_col <- tail(names(factorized_data), 1)
            other_cols <- setdiff(names(factorized_data), last_col)
            model_type <- "Random Forest" # input$model_type
            if (model_type == "Random Forest") {
                tagList(
                    fluidRow(
                        column(
                            width = 6,
                            selectizeInput(
                                "dep_var", "Select dependent variable",
                                choices = names(factorized_data), selected = last_col,
                                width = "100%"),
                            sliderInput("split", "Train/Validation split",
                                        min = 0.5, max = 0.95, value = 0.8, step = 0.01,
                                        width = "100%"),
                            sliderInput("ntree", "Number of trees",
                                        min = 10, max = 500, value = 10, step = 10,
                                        width = "100%"),
                            actionButton("create_model", "Create Model",
                                         width = "100%")
                        ),
                        column(
                            width = 6,
                            selectizeInput(
                                "indep_vars", "Select independent variables",
                                choices = other_cols, selected = other_cols, multiple = TRUE,
                                width = "100%"),
                        )
                    )
                )
            } else {
                tags$div(
                    style = "margin-top: 20px;",
                    tags$strong("Neural Network configuration is not yet implemented."))
            }
        })
    })

    observeEvent(input$dep_var, {
        updateSelectizeInput(
            inputId = "indep_vars",
            choices = setdiff(names(factorized_data), input$dep_var),
            selected = setdiff(names(factorized_data), input$dep_var))
    })

    observeEvent(c(input$dep_var, input$indep_vars), {
        comb_sizes <- lapply(factorized_data[,c(input$dep_var, input$indep_vars)], function(col) length(levels(col)))
        total_combinations <- Reduce(`*`, comb_sizes)
        output$mlmorph_size <- renderUI(
            tags$p(sprintf("The number of combinations: %s.",
                           total_combinations), class="mlmorph_size")
        )
    })

    observeEvent(input$create_model, {
        rf_model <<- MLmorph::create_rf_model(
            data = factorized_data,
            dependent = input$dep_var,
            independent = input$indep_vars,
            train_validate_split = input$split,
            ntree = input$ntree,
            shiny = TRUE
        )

        output$model_outputs <- renderUI({
            acc <- rf_model$model_performance_on_test$overall
            tagList(
                tags$h3("Variable importance (on train data)"),
                reactableOutput("var_imp_table"),
                tags$h3("Model performance (on validation data)"),
                tags$strong("Model accuracy"),
                tags$p(sprintf(
                    "The model accuracy on validation data is %0.2f%% versus %0.02f%% no information rate (p-value approx. %0.4f).",
                    acc["Accuracy"]*100,
                    acc["AccuracyNull"]*100,
                    acc["AccuracyPValue"])),
                tags$strong("Confusion table"),
                plotlyOutput("conf_matrix_plot"),
                tags$h3("Model calibration (on validation data)"),
                tags$strong("Out-of-bag error vs number of trees (on train data)"),
                plotlyOutput("error_plot")
            )
        })

        output$var_imp_table <- renderReactable({
            reactable(rf_model$variables_importance, striped = TRUE)
        })

        output$conf_matrix_plot <- renderPlotly({
            conf_df <- as.data.frame(rf_model$model_performance_on_test$table)
            conf_wide <- tidyr::pivot_wider(
                conf_df, names_from = Reference,
                values_from = Freq, values_fill = 0)
            mat <- as.matrix(conf_wide[, -1, drop = FALSE])
            plot_ly(
                x = colnames(mat), y = conf_wide$Prediction, z = mat,
                type = "heatmap", colorscale = "Greys", reversescale = TRUE,
                hovertemplate = "Prediction %{y}<br>Reference %{x}<br>Count %{z}<extra></extra>") %>%
                layout(
                    xaxis = list(title = "Reference", type = "category"),
                    yaxis = list(title = "Prediction", type = "category"))
        })

        output$error_plot <- renderPlotly({
            err <- rf_model$model$err.rate[, 1]
            plot_ly(
                x = seq_along(err), y = err,
                type = "scatter", mode = "lines+markers",
                name = "OOB Error") %>%
                layout(
                    xaxis = list(title = "Number of Trees"),
                    yaxis = list(title = "OOB Error Rate"))
        })
        shinyjs::enable(selector = 'a[data-value="> Create morphospace"]')
    })

    observeEvent(input$create_morphospace, {
        restrictions <<- list(exclude_simulated = FALSE)
        full_morphospace <<- MLmorph::create_morphospace(
            factorized_data, rf_model$model, shiny = TRUE)
        sim_ratio <- mean(full_morphospace$purely_simulated)
        output$morphospace_outputs <- renderUI({
            tagList(
                tags$p(sprintf(
                    "Morphospace size: %s (%0.2f%% purely simulated).",
                    nrow(full_morphospace$morphospace), sim_ratio * 100)),
                plotlyOutput("morphospace_hist")
            )
        })
        output$morphospace_hist <- renderPlotly({
            dep_var <- full_morphospace$dependent
            levs <- levels(full_morphospace$morphospace[[dep_var]])
            g <- ggplot(
                full_morphospace$morphospace, aes(x = calculated)) +
                geom_bar() +
                facet_wrap(~.data[[dep_var]], ncol = 1) +
                labs(x = "probability", y = "combinations count")
            ggplotly(g, height = length(levs) * 200)
        })
        update_morphospace()
        output$morphmodel <- renderUI(make_morphmodel())
        shinyjs::enable(selector = 'a[data-value="> Visualize MLmorph model"]')
        shinyjs::enable(selector = 'a[data-value="> Export MLmorph model"]')
    })

    output$download_model <- downloadHandler(
        filename = function() {
            sprintf("mlmorph_%s.mlmorph",
                    format(Sys.time(), "%Y-%m-%d_%H-%M"))
        },
        content = function(file) {
            saveRDS(full_morphospace, file)
        })

    output$download_factorized <- downloadHandler(
        filename = function() {
            sprintf("factorized_data_%s.csv",
                    format(Sys.time(), "%Y-%m-%d_%H-%M"))
        },
        content = function(file) {
            write.csv(factorized_data, file, row.names = FALSE, fileEncoding = "UTF-8")
        })

    observeEvent(input$file_load, {
        restrictions <<- list(exclude_simulated = FALSE)
        full_morphospace <<- readRDS(input$file_load$datapath)
        output$import_results <- renderPrint(HTML(sprintf(
            "<p>Imported %d combinations MLmorph model</p>",
            nrow(full_morphospace$morphospace))))
        update_morphospace()
        output$morphmodel <- renderUI(make_morphmodel())
        shinyjs::enable(selector = 'a[data-value="> Visualize MLmorph model"]')
    })

    observeEvent(input$exclude_simulated, {
        req(full_morphospace)
        restrictions$exclude_simulated <<- input$exclude_simulated
        update_morphospace()
        output$morphmodel <- renderUI(make_morphmodel())
    })

    observeEvent(input$toggle_restrictions, {
        ind <- strsplit(input$toggle_restrictions, ";")[[1]] %>%
            strsplit(":") %>% unlist() %>% {
                as.numeric(c(.[2], .[4])) }
        restrictions[[as.character(ind[1])]] <<- if (!is.null(
            restrictions[[as.character(ind[1])]]) &&
            restrictions[[as.character(ind[1])]] == ind[2]) {
            NULL
        } else {
            ind[2]
        }
        update_morphospace()
        output$morphmodel <- renderUI(make_morphmodel())
    })
    observeEvent(input$apply_factorization, {
        req(imported_data)
        df <- imported_data
        fact_df <- df

        `%||%` <- function(a, b) if (!is.null(a)) a else b

        for (i in seq_along(df)) {
            varname <- names(df)[i]
            col <- df[[i]]
            method <- input[[paste0("fact_method_", i)]]

            fact_df[[varname]] <- switch(method,
                                   "nicely" = factorize_nicely_vector(col),
                                   "numeric_bins" = factorize_numeric_vector(
                                       col,
                                       method = "equal_bins",
                                       breaks_no = input[[paste0("breaks_no_", i)]] %||% 5
                                   ),
                                   "numeric_distance" = factorize_numeric_vector(
                                       col,
                                       method = "equal_distance",
                                       breaks_no = input[[paste0("breaks_no_", i)]] %||% 5
                                   ),
                                   "custom_breaks" = {
                                       brk <- as.numeric(strsplit(
                                           input[[paste0("custom_breaks_", i)]], ",")[[1]])
                                       lbl_raw <- input[[paste0("custom_labels_", i)]]
                                       lbl <- if (nzchar(lbl_raw)) strsplit(lbl_raw, ",")[[1]] else NULL
                                       factorize_numeric_vector(
                                           col,
                                           method = "custom_breaks",
                                           custom_breaks = brk,
                                           custom_labels = lbl
                                       )
                                   },
                                   "binary" = factorize_binary_vector(col),
                                   "character" = factorize_character_vector(col),
                                   "identity" = factorize_identity(col),
                                   "drop" = NULL,
                                   col
            )
        }
        fact_df <- na.omit(fact_df)
        factorized_data <<- fact_df
        comb_sizes <- lapply(fact_df, function(col) length(levels(col)))
        total_combinations <- Reduce(`*`, comb_sizes)
        output$configure_preview <- renderUI({
            factorized <- lapply(fact_df, levels) %>% setNames(colnames(fact_df))
            tagList(
                tags$p(sprintf(
                    "The number of combinations: %s. Factorized rows: %s.",
                    total_combinations,
                    nrow(factorized_data)), class="mlmorph_size"),
                make_html_table_from_list(factorized)
            )
        })
        output$mlmorph_size <- renderUI(
            tags$p(sprintf("The number of combinations: %s.",
                            total_combinations), class="mlmorph_size")
        )
        output$rf_controls <- renderUI({
            last_col <- tail(names(factorized_data), 1)
            other_cols <- setdiff(names(factorized_data), last_col)
            model_type <- "Random Forest" # input$model_type
            if (model_type == "Random Forest") {
                tagList(
                    fluidRow(
                        column(
                            width = 6,
                            selectizeInput(
                                "dep_var", "Select dependent variable",
                                choices = names(factorized_data), selected = last_col,
                                width = "100%"),
                            sliderInput("split", "Train/Validation split",
                                        min = 0.5, max = 0.95, value = 0.8, step = 0.01,
                                        width = "100%"),
                            sliderInput("ntree", "Number of trees",
                                        min = 10, max = 500, value = 10, step = 10,
                                        width = "100%"),
                            actionButton("create_model", "Create Model",
                                         width = "100%")
                        ),
                        column(
                            width = 6,
                            selectizeInput(
                                "indep_vars", "Select independent variables",
                                choices = other_cols, selected = other_cols, multiple = TRUE,
                                width = "100%"),
                        )
                    )
                )
            } else {
                tags$div(
                    style = "margin-top: 20px;",
                    tags$strong("Neural Network configuration is not yet implemented."))
            }
        })
        shinyjs::enable(selector = 'a[data-value="> Create ML model"]')
    })
}
