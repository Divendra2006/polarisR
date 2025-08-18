#' Run NLDR Visualization Tool
#'
#' Launches the interactive NLDR (Non-Linear Dimensionality Reduction) Visualization Tool 
#' as a Shiny application. This application provides a comprehensive platform for exploring
#' high-dimensional datasets using dimensionality reduction techniques such as t-SNE and UMAP,
#' with additional features for quality assessment, dynamic tours, and method comparison.
#'
#' @details 
#' The NLDR Visualization Tool includes the following key features:
#' \itemize{
#'   \item **Dataset Management**: Load custom CSV files or use built-in example datasets
#'   \item **Dimensionality Reduction**: Apply t-SNE or UMAP with customizable parameters
#'   \item **Interactive Visualization**: Create interactive plots with brushing and linking
#'   \item **Dynamic Tours**: Explore high-dimensional data through animated projections
#'   \item **Quality Assessment**: Use quollr package for embedding quality diagnostics
#'   \item **Method Comparison**: Compare different NLDR configurations side-by-side
#'   \item **Reproducibility**: Set random seeds and save/load analysis results
#' }
#'
#' The application is organized into several tabs:
#' \itemize{
#'   \item **Dataset Preview**: Upload data and preview dataset characteristics
#'   \item **Dataset Visualization**: Apply NLDR methods and visualize results
#'   \item **Dynamic Tour**: Explore high-dimensional structure through animated tours
#'   \item **Diagnosing**: Assess embedding quality using binwidth optimization and quollr
#'   \item **Method Comparison**: Compare different NLDR configurations and visualizations
#' }
#'
#' @param ... Additional arguments passed to \code{\link[shiny]{shinyApp}}, such as:
#'   \itemize{
#'     \item \code{host}: IP address to bind to (default: "127.0.0.1")
#'     \item \code{port}: Port number to use (default: random available port)
#'     \item \code{launch.browser}: Whether to launch browser automatically (default: TRUE)
#'     \item \code{options}: List of additional options for the app
#'   }
#'
#' @return A \code{\link[shiny]{shiny.appobj}} object representing the Shiny application.
#'   When run, this object launches the interactive NLDR visualization interface in a web browser.
#'
#' @note 
#' The application requires several packages to be installed:
#' \itemize{
#'   \item Core visualization: \code{shiny}, \code{bslib}, \code{plotly}, \code{DT}
#'   \item NLDR methods: \code{Rtsne}, \code{umap}
#'   \item Quality assessment: \code{quollr}
#'   \item Dynamic tours: \code{detourr}, \code{tourr}
#'   \item Data processing: \code{dplyr}, \code{scales}, \code{crosstalk}
#'   \item Parallel processing: \code{future}
#' }
#'
#' @seealso 
#' \itemize{
#'   \item \code{\link{nldr_viz_ui}} for UI structure
#'   \item \code{\link{nldr_viz_server}} for server logic
#'   \item \code{\link{load_custom_datasets}} for data loading utilities
#' }
#'
#' @import shiny
#' @export
#'
#' @examples
#' \dontrun{
#' # Launch the application with default settings
#' run_nldr_viz()
#' 
#' # Launch on a specific port
#' run_nldr_viz(port = 3838)
#' 
#' # Launch without opening browser automatically
#' run_nldr_viz(launch.browser = FALSE)
#' 
#' # Store the app object for later use
#' app <- run_nldr_viz()
#' # The app can then be deployed or run on different configurations
#' }
#'
#' @author GSoC Contributor
#' @keywords visualization dimensionality-reduction interactive shiny
run_nldr_viz <- function(...) {
  shiny::shinyApp(ui = nldr_viz_ui(), server = nldr_viz_server, ...)
}
