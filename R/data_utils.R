#' Load Custom Datasets
#'
#' Loads datasets from the package data or uses built-in datasets
#'
#' @return A list of datasets
#' @keywords internal
load_custom_datasets <- function() {
  datasets <- list()

  tryCatch({
    load(system.file("data", "four_clusters.rda", package = "nldrViz"), envir = environment())
    if(exists("four_clusters", envir = environment())) {
      datasets[["four_clusters"]] <- four_clusters
    }
  }, error = function(e) {
    warning("Could not load four_clusters data: ", e$message)
  })

  tryCatch({
    load(system.file("data", "pdfsense.rda", package = "nldrViz"), envir = environment())
    if(exists("pdfsense", envir = environment())) {
      datasets[["pdfsense"]] <- pdfsense
    }
  }, error = function(e) {
    warning("Could not load pdfsense data: ", e$message)
  })

  tryCatch({
    load(system.file("data", "fake_trees.rda", package = "nldrViz"), envir = environment())
    if(exists("fake_trees", envir = environment())) {
      datasets[["trees"]] <- fake_trees
    }
  }, error = function(e) {
    warning("Could not load fake_trees data: ", e$message)
  })

  return(datasets)
}
