library(shiny)
library(shinyjs)
library(shinyFiles)
library(bslib)
library(thematic)
library(plotly)
library(reactable)

theme <- bs_theme_update(bs_theme(version = 5), bg = "#ffffff", fg = "#000",
                         primary = "#ff4040", secondary = "#022CFA", font_scale = NULL,
                         bootswatch = "pulse")

ui <- fluidPage(
	useShinyjs(),
	navbarPage(
		# includeScript("www/custom.js"),
		includeCSS("www/main.css"),
		theme = theme,
		title = "MLmorph",
		tabPanel(
		    "> Load data", class = "load-data",
			bslib::layout_columns(
				h2("Data loading tab"),
				fileInput("file_import",
	                "Import file (xlsx / json / csv)",
	                accept = c(".xlsx", ".json", ".csv"),
	                width = "100%",
	                multiple = FALSE
	            ),
				p("or", class = "center"),
				fileInput("file_load",
				          "Load MLmorph model",
				          accept = c(".mlmorph"),
				          width = "100%",
				          multiple = FALSE
				),
	            htmlOutput("import_results"),
				col_widths = bslib::breakpoints(
    				sm = c(12),
    				md = c(12, -2, 3, 2, 3, -2, 12)
    				# xl = c(12)
				)
			)
		),
		tabPanel(
		    "> Configure data", class = "configure-data",
            layout_columns(
                h2("Data configuring tab"),
                uiOutput("factorization_controls"),
                actionButton("apply_factorization", "Apply factorization"),
                downloadButton("download_factorized", "Download factorized"),
                uiOutput("configure_preview"),
                col_widths = breakpoints(
                    sm = c(12),
                    md = c(12, -4, 4, -4, -4, 2, 2, -4, 12)
                    # xl = c(12)
                )
            )
		),
		tabPanel(
		    "> Create ML model", class = "create-model",
            layout_columns(
                h2("Create the model"),
                selectizeInput(
                    inputId = "model_type",
                    label = "Select model type",
                    choices = c("Random Forest", "Neural Network"),
                    selected = "Random Forest",
                    multiple = FALSE
                ),
                uiOutput("rf_controls"),
                uiOutput("mlmorph_size"),
                verbatimTextOutput("model_config"),
                uiOutput("model_outputs"),
                col_widths = breakpoints(
                    sm = c(12, 6, 6, 12, 12),
                    md = c(12, -3, 3, 3, -3, 12, 12)
                    # xl = c(12)
                )
            )
		),
		tabPanel(
		    "> Create morphospace", class = "create-morphospace",
            layout_columns(
                h2("Create morphospace"),
                shiny::actionButton("create_morphospace", "Create morphospace"),
                uiOutput("morphospace_outputs"),
                col_widths = breakpoints(
                    sm = c(12),
                    md = c(12),
                    xl = c(12)
                )
            )
		),
		tabPanel(
		    "> Visualize MLmorph model", class = "visualize-mlmorph",
            layout_columns(
                h2("Visualize MLmorph model"),
                checkboxInput("exclude_simulated",
                              "Exclude purely simulated?",
                              value = FALSE),
                uiOutput("morphmodel"),
                col_widths = breakpoints(
                    sm = c(12),
                    md = c(12),
                    xl = c(12)
                )
            )
		),
		tabPanel(
		    "> Export MLmorph model", class = "export-mlmorph",
            layout_columns(
                h2("Export model"),
                downloadButton("download_model"),
                col_widths = breakpoints(
                    sm = c(12),
                    md = c(12),
                    xl = c(12)
                )
            )
		)
	)
)
