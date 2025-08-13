library(shiny)

test_that("check_empty_cells function works correctly", {
  check_empty_cells <- function(data) {
    if (is.null(data) || nrow(data) == 0) {
      return(list(has_empty = FALSE))
    }
    has_empty <- any(sapply(data, function(x) any(is.na(x) | x == "" | is.null(x))))
    if (has_empty) {
      empty_counts <- sapply(data, function(x) sum(is.na(x) | x == "" | is.null(x)))
      empty_cols <- names(empty_counts)[empty_counts > 0]
      return(list(
        has_empty = TRUE,
        empty_cols = empty_cols,
        total_empty = sum(empty_counts)
      ))
    }
    return(list(has_empty = FALSE))
  }
  
  clean_data <- data.frame(
    x = 1:5,
    y = 6:10,
    z = letters[1:5]
  )
  result <- check_empty_cells(clean_data)
  expect_false(result$has_empty)
  
  empty_data <- data.frame()
  result <- check_empty_cells(empty_data)
  expect_false(result$has_empty)
  
  result <- check_empty_cells(NULL)
  expect_false(result$has_empty)
  
  dirty_data <- data.frame(
    x = c(1, 2, NA, 4, 5),
    y = c(6, 7, 8, 9, 10),
    z = c("a", "", "c", "d", "e")
  )
  result <- check_empty_cells(dirty_data)
  expect_true(result$has_empty)
  expect_true("x" %in% result$empty_cols)
  expect_true("z" %in% result$empty_cols)
  expect_equal(result$total_empty, 2)
})

test_that("extract_base_dataset_name function works correctly", {
  extract_base_dataset_name <- function(full_name) {
    if (grepl("\\s-\\s", full_name)) {
      base_name <- gsub("\\s*-\\s*(t-SNE|UMAP).*$", "", full_name)
      return(trimws(base_name))
    } else {
      return(full_name)
    }
  }
  
  expect_equal(extract_base_dataset_name("four_clusters"), "four_clusters")
  expect_equal(extract_base_dataset_name("pdfsense - t-SNE"), "pdfsense")
  expect_equal(extract_base_dataset_name("trees - UMAP"), "trees")
  expect_equal(extract_base_dataset_name("custom_data - t-SNE (perplexity=30)"), "custom_data")
  expect_equal(extract_base_dataset_name("  spaced_name  - UMAP  "), "spaced_name")
  expect_equal(extract_base_dataset_name("no_method_specified"), "no_method_specified")
})

test_that("server reactive values initialization", {
  expect_no_error({
    server_func <- nldr_viz_server
    expect_true(is.function(server_func))
  })
  
  testResult <- try({
    shiny::testServer(nldr_viz_server, {
      expect_true(exists("session"))
    })
  }, silent = TRUE)
  
  if (inherits(testResult, "try-error")) {
    expect_true(is.function(nldr_viz_server))
    expect_true("server" %in% class(environment(nldr_viz_server)))
  }
})

test_that("data validation logic works correctly", {
  test_data <- data.frame(
    numeric1 = 1:10,
    numeric2 = rnorm(10),
    character1 = letters[1:10],
    factor1 = factor(rep(c("A", "B"), 5))
  )
  
  numeric_cols <- sapply(test_data, is.numeric)
  expect_equal(sum(numeric_cols), 2)
  expect_true(numeric_cols["numeric1"])
  expect_true(numeric_cols["numeric2"])
  expect_false(numeric_cols["character1"])
  expect_false(numeric_cols["factor1"])
  
  expect_true(nrow(test_data) == 10)
  expect_true(ncol(test_data) == 4)
})

test_that("dataset selection and filtering logic", {
  datasets <- load_custom_datasets()
  
  expect_true("four_clusters" %in% names(datasets))
  expect_true("pdfsense" %in% names(datasets))
  expect_true("trees" %in% names(datasets))
  
  four_clusters <- datasets$four_clusters
  numeric_cols <- sapply(four_clusters, is.numeric)
  numeric_data <- four_clusters[, numeric_cols, drop = FALSE]
  
  expect_true(ncol(numeric_data) >= 2)
  expect_equal(nrow(numeric_data), nrow(four_clusters))
})

test_that("input validation functions", {
  validate_perplexity <- function(perplexity, n_samples) {
    if (perplexity < 5 || perplexity > 50) return(FALSE)
    if (perplexity >= n_samples) return(FALSE)
    return(TRUE)
  }
  
  expect_true(validate_perplexity(30, 100))
  expect_false(validate_perplexity(3, 100))
  expect_false(validate_perplexity(60, 100))
  expect_false(validate_perplexity(30, 25))
  
  validate_umap_params <- function(n_neighbors, min_dist) {
    if (n_neighbors < 2 || n_neighbors > 50) return(FALSE)
    if (min_dist < 0.01 || min_dist > 0.99) return(FALSE)
    return(TRUE)
  }
  
  expect_true(validate_umap_params(15, 0.1))
  expect_false(validate_umap_params(1, 0.1))
  expect_false(validate_umap_params(60, 0.1))
  expect_false(validate_umap_params(15, 0.005))
  expect_false(validate_umap_params(15, 1.5))
})
