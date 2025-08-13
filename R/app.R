#' Run NLDR Visualization Tool
#'
#' Launches the NLDR Visualization Tool Shiny application
#'
#' @param ... Additional arguments passed to shiny::shinyApp
#' @import shiny
#' @return Shiny application object
#' @export
#'
#' @examples
#' \dontrun{
#' run_nldr_viz()
#' }
run_nldr_viz <- function(...) {
  shiny::shinyApp(ui = nldr_viz_ui(), server = nldr_viz_server, ...)
}
