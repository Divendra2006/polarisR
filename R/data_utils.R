#' Load Built-in Datasets for NLDR Visualization
#'
#' Loads and returns a named list of built-in example datasets that are included
#' with the polarisR package. These datasets are specifically chosen to demonstrate
#' different characteristics and challenges in non-linear dimensionality reduction
#' analysis and visualization.
#'
#' @return A named list containing the successfully loaded datasets. Each element
#'   is a data.frame with the following structure:
#'   \itemize{
#'     \item \code{$four_clusters}: data.frame with cluster analysis data
#'     \item \code{$pdfsense}: data.frame with high-dimensional physics data
#'     \item \code{$trees}: data.frame with tree-structured synthetic data
#'   }
#'
#'   If any dataset fails to load, a warning is issued but the function continues,
#'   returning only the successfully loaded datasets. An empty list is returned
#'   if no datasets can be loaded.
#'
#' @section Error Handling:
#' The function includes robust error handling:
#' \itemize{
#'   \item Each dataset is loaded in a separate \code{tryCatch} block
#'   \item Loading failures generate warnings rather than stopping execution
#'   \item Existence checks ensure datasets are properly loaded before inclusion
#'   \item Environment management prevents namespace pollution
#' }
#'
#' @section Usage in Application:
#' This function is typically called during application initialization to populate
#' the example dataset dropdown in the UI. The returned list is stored in a reactive
#' value and can be dynamically extended with user-uploaded datasets.
#'
#' @note
#' The function uses \code{\link[utils]{data}} to load datasets from the package
#' namespace. If the polarisR package is not properly installed or the data files
#' are missing, warnings will be generated for the affected datasets.
#'
#' @seealso
#' \itemize{
#'   \item \code{\link{four_clusters}} for four_clusters dataset documentation
#'   \item \code{\link{pdfsense}} for pdfsense dataset documentation
#'   \item \code{\link{fake_trees}} for fake_trees dataset documentation
#'   \item \code{\link[utils]{data}} for dataset loading mechanism
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Load all available datasets
#' datasets <- load_custom_datasets()
#' names(datasets)  # Shows available dataset names
#'
#' # Check what was successfully loaded
#' if ("four_clusters" %in% names(datasets)) {
#'   dim(datasets$four_clusters)
#'   head(datasets$four_clusters)
#' }
#'
#' # Use in Shiny application initialization
#' custom_datasets <- shiny::reactiveVal(load_custom_datasets())
#' available_datasets <- shiny::reactiveVal(
#'   c("None", names(load_custom_datasets()))
#' )
#' }
#'
#' @author GSoC Contributor
#' @keywords data datasets examples loading
load_custom_datasets <- function() {
  datasets <- list()

  # Load four_clusters dataset
  tryCatch({
    data("four_clusters", package = "polarisR", envir = environment())
    if(exists("four_clusters", envir = environment())) {
      datasets[["four_clusters"]] <- get("four_clusters", envir = environment())
    }
  }, error = function(e) {
    warning("Could not load four_clusters data: ", e$message)
  })

  # Load pdfsense dataset
  tryCatch({
    data("pdfsense", package = "polarisR", envir = environment())
    if(exists("pdfsense", envir = environment())) {
      datasets[["pdfsense"]] <- get("pdfsense", envir = environment())
    }
  }, error = function(e) {
    warning("Could not load pdfsense data: ", e$message)
  })

  # Load fake_trees dataset as "trees"
  tryCatch({
    data("fake_trees", package = "polarisR", envir = environment())
    if(exists("fake_trees", envir = environment())) {
      datasets[["trees"]] <- get("fake_trees", envir = environment())
    }
  }, error = function(e) {
    warning("Could not load fake_trees data: ", e$message)
  })

  return(datasets)
}
