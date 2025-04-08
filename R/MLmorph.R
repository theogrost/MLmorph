# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

MLmorph <- function(host = "127.0.0.1",
                    port = NULL,
                    launch.browser = TRUE,
                    maxUploadSize = 20 * 1024){

    shiny::shinyOptions(maxUploadSize = maxUploadSize)
    shiny::runApp(system.file("MLmorph",
                              package="MLmorph"),
                  launch.browser = launch.browser,
                  port = port,
                  host = base::getOption("shiny.host", host))

}

