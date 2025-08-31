#' Run NLDR Visualization Tool
#'
#' Launches the interactive NLDR (Non-Linear Dimensionality Reduction) Visualization Tool
#' as a Shiny application for exploring high-dimensional datasets using t-SNE and UMAP.
#'
#' @param ... Additional arguments passed to \code{\link[shiny]{shinyApp}}
#'
#' @details
#' The application includes five main tabs:
#' \itemize{
#'   \item **Dataset Preview**: Upload data and preview dataset characteristics
#'   \item **Non-linear dimension reduction (NLDR)**: Apply t-SNE or UMAP methods
#'   \item **Dynamic Tour**: Explore high-dimensional structure through animated projections
#'   \item **Diagnosing**: Assess embedding quality using quollr package
#'   \item **2-D Layout Comparison**: Compare different NLDR configurations
#' }
#'
#' @return A Shiny application object that launches the NLDR visualization interface.
#'
#' @seealso
#' \code{\link{nldr_viz_ui}}, \code{\link{nldr_viz_server}}
#'
#' @import shiny
#' @export
#'
run_nldr_viz <- function(...) {
  shiny::shinyApp(ui = nldr_viz_ui(), server = nldr_viz_server, ...)
}
