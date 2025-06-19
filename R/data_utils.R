#' Load Custom Datasets
#'
#' Loads datasets from the data-raw folder or uses built-in datasets
#'
#' @return A list of datasets
#' @keywords internal
load_custom_datasets <- function() {
  datasets <- list()

  # Try to load four_clusters.R
  tryCatch({
    source(system.file("data-raw", "four_clusters.R", package = "nldrViz"), local = TRUE)
    if(exists("four_clusters")) {
      datasets[["four_clusters"]] <- four_clusters
    }
  }, error = function(e) {
    warning("Could not load four_clusters.R: ", e$message)
  })

  # Try to load pdfsense.R
  tryCatch({
    source(system.file("data-raw", "pdfsense.R", package = "nldrViz"), local = TRUE)
    if(exists("pdfsense")) {
      datasets[["pdfsense"]] <- pdfsense
    }
  }, error = function(e) {
    warning("Could not load pdfsense.R: ", e$message)
  })

  # Try to load trees.R
  tryCatch({
    source(system.file("data-raw", "trees.R", package = "nldrViz"), local = TRUE)
    if(exists("fake_trees")) {
      datasets[["trees"]] <- fake_trees
    }
  }, error = function(e) {
    warning("Could not load trees.R: ", e$message)
    # Try using the built-in trees dataset as fallback
    datasets[["trees"]] <- datasets::trees
  })

  return(datasets)
}
