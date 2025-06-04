#' Launch NLDR Diagnostic Tool
#' @export
run_app <- function() {
  require(shiny)
  require(bslib)
  require(plotly)

  options(bslib.precompiled = FALSE)

  shinyApp(
    ui = app_ui(),
    server = app_server
  )
}
