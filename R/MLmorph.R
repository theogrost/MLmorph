#' Launch the MLmorph Shiny app
#'
#' @param host Host interface to bind (default \code{"127.0.0.1"}).
#' @param port Integer port or \code{NULL} to auto-select.
#' @param launch.browser Logical; open in a browser. Default \code{TRUE}.
#' @param maxUploadSize Maximum request size in bytes; sets
#'   \code{options(shiny.maxRequestSize = ...)}. Default \code{200 * 1024^2}.
#'
#' @return The value returned by \code{shiny::runApp()}.
#' @seealso \code{\link[shiny]{runApp}}
#' @export
#'
#' @examples
#' \dontrun{
#' # Launch app from the installed package
#' MLmorph()
#' }
MLmorph <- function(host = "127.0.0.1",
                    port = NULL,
                    launch.browser = TRUE,
                    maxUploadSize = 200 * 1024^2){
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
#' @examples
#' \dontrun{
#' # From package root during development
#' MLmorph_live()
#' }
MLmorph_live <- function() {
    options(shiny.maxRequestSize = 200 * 1024^2)
    shiny::shinyOptions(maxUploadSize = 200 * 1024^2)
    shiny::runApp(paste0(getwd(),"/inst/MLmorph"), launch.browser = TRUE, host = "127.0.0.1")
}
