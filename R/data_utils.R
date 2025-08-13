#' Load Custom Datasets
#'
#' Loads datasets from the package data or uses built-in datasets
#'
#' @return A list of datasets
#' @export
load_custom_datasets <- function() {
  datasets <- list()

  # Load four_clusters dataset
  tryCatch({
    data("four_clusters", package = "Polaris", envir = environment())
    if(exists("four_clusters", envir = environment())) {
      datasets[["four_clusters"]] <- get("four_clusters", envir = environment())
    }
  }, error = function(e) {
    warning("Could not load four_clusters data: ", e$message)
  })

  # Load pdfsense dataset
  tryCatch({
    data("pdfsense", package = "Polaris", envir = environment())
    if(exists("pdfsense", envir = environment())) {
      datasets[["pdfsense"]] <- get("pdfsense", envir = environment())
    }
  }, error = function(e) {
    warning("Could not load pdfsense data: ", e$message)
  })

  # Load fake_trees dataset as "trees"
  tryCatch({
    data("fake_trees", package = "Polaris", envir = environment())
    if(exists("fake_trees", envir = environment())) {
      datasets[["trees"]] <- get("fake_trees", envir = environment())
    }
  }, error = function(e) {
    warning("Could not load fake_trees data: ", e$message)
  })

  return(datasets)
}
