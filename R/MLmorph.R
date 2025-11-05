#' Launch the MLmorph shiny app
#'
#' @param host Host interface to bind (default \code{"127.0.0.1"}).
#' @param port Integer port or \code{NULL} to auto-select.
#' @param launch.browser Logical; open in a browser. Default \code{TRUE}.
#' @param maxUploadSize Maximum request size in bytes; sets
#'   \code{options(shiny.maxRequestSize = ...)}. Default \code{200 * 1024^2}.
#'
#' @return The value returned by \link[shiny]{runApp}.
#' @seealso \code{\link[shiny]{runApp}}
#' @export
#'
#' @examples
#' if(interactive()){
#' MLmorph()
#' }
MLmorph <- function(host = "127.0.0.1",
                    port = NULL,
                    launch.browser = TRUE,
                    maxUploadSize = 200 * 1024^2){
    MLmorph:::MLmorph_dependencies()
    options(shiny.maxRequestSize = maxUploadSize)
    shiny::shinyOptions(maxUploadSize = maxUploadSize)
    shiny::runApp(system.file("MLmorph",
                              package="MLmorph"),
                  launch.browser = launch.browser,
                  port = port,
                  host = base::getOption("shiny.host", host))

}

#' Launch MLmorph from the source tree (development helper)
#'
#' @keywords internal
MLmorph_live <- function() {
    MLmorph:::MLmorph_dependencies()
    options(shiny.maxRequestSize = 200 * 1024^2)
    shiny::shinyOptions(maxUploadSize = 200 * 1024^2)
    shiny::runApp(paste0(getwd(),"/inst/MLmorph"), launch.browser = TRUE, host = "127.0.0.1")
}

#' Iterate through app libraries and functions
#'
#' This function references packages and functions used in the MLmorph Shiny app,
#' located in `inst/`, to prevent `R CMD check`.
#'
#' @keywords internal
#' @return NULL (invisible)
#' @export
MLmorph_dependencies <- function() {
    # Shiny
    shiny::fluidPage
    shiny::navbarPage
    shiny::fileInput
    shiny::actionButton
    shiny::selectizeInput
    shiny::sliderInput
    shiny::checkboxInput
    shiny::downloadButton
    shiny::uiOutput
    shiny::renderUI
    shiny::verbatimTextOutput
    shiny::htmlOutput
    shiny::observeEvent
    shiny::plotOutput
    shiny::renderPlot

    # shinyjs
    shinyjs::useShinyjs
    shinyjs::disable
    shinyjs::enable

    # shinyFiles
    shinyFiles::shinyFileChoose

    # bslib
    bslib::bs_theme
    bslib::bs_theme_update
    bslib::layout_columns
    bslib::breakpoints

    # htmltools
    htmltools::HTML
    htmltools::tags

    # plotly
    plotly::plot_ly
    plotly::layout

    # reactable
    reactable::reactable

    # ggplot2
    ggplot2::ggplot
    ggplot2::geom_bar
    ggplot2::facet_wrap
    ggplot2::labs

    # dplyr
    dplyr::filter

    # tidyr
    tidyr::pivot_wider

    # magrittr
    magrittr::`%>%`
    magrittr::set_colnames

    # MLmorph
    MLmorph::load_data
    MLmorph::create_rf_model
    MLmorph::create_morphospace
    MLmorph::factorize_nicely_dataframe
    MLmorph::factorize_numeric_vector
    MLmorph::factorize_binary_vector
    MLmorph::factorize_character_vector
    MLmorph::factorize_identity

    invisible(NULL)
}
