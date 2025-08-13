test_that("package datasets are accessible", {
  expect_no_error(data("four_clusters", package = "Polaris"))
  expect_no_error(data("pdfsense", package = "Polaris"))
  expect_no_error(data("fake_trees", package = "Polaris"))
  
  data("four_clusters", package = "Polaris")
  expect_true(exists("four_clusters"))
  expect_s3_class(four_clusters, "data.frame")
  
  data("pdfsense", package = "Polaris")
  expect_true(exists("pdfsense"))
  expect_s3_class(pdfsense, "data.frame")
  
  data("fake_trees", package = "Polaris")
  expect_true(exists("fake_trees"))
  expect_s3_class(fake_trees, "data.frame")
})

test_that("dataset characteristics are appropriate for NLDR", {
  data("four_clusters", package = "Polaris")
  data("pdfsense", package = "Polaris")
  data("fake_trees", package = "Polaris")
  
  expect_true("cluster" %in% names(four_clusters))
  expect_gt(length(unique(four_clusters$cluster)), 1)
  
  numeric_cols_fc <- sum(sapply(four_clusters, is.numeric))
  numeric_cols_pdf <- sum(sapply(pdfsense, is.numeric))
  numeric_cols_trees <- sum(sapply(fake_trees, is.numeric))
  
  expect_gte(numeric_cols_fc, 2)
  expect_gte(numeric_cols_pdf, 2)
  expect_gte(numeric_cols_trees, 2)
  
  expect_gte(nrow(four_clusters), 10)
  expect_gte(nrow(pdfsense), 10)
  expect_gte(nrow(fake_trees), 10)
  
  expect_false(any(is.na(four_clusters$x1)))
  expect_false(any(is.na(four_clusters$x2)))
  expect_false(any(is.na(four_clusters$cluster)))
})

test_that("data transformations preserve structure", {
  data("four_clusters", package = "Polaris")
  
  original_nrow <- nrow(four_clusters)
  original_ncol <- ncol(four_clusters)
  
  numeric_data <- four_clusters[sapply(four_clusters, is.numeric)]
  expect_equal(nrow(numeric_data), original_nrow)
  expect_gte(ncol(numeric_data), 1)
  
  scaled_data <- scale(numeric_data)
  expect_equal(nrow(scaled_data), original_nrow)
  expect_equal(ncol(scaled_data), ncol(numeric_data))
})

test_that("load_custom_datasets integrates with package data", {
  custom_datasets <- load_custom_datasets()
  
  expect_true("four_clusters" %in% names(custom_datasets))
  expect_true("pdfsense" %in% names(custom_datasets))
  expect_true("trees" %in% names(custom_datasets))
  
  data("four_clusters", package = "Polaris")
  expect_equal(nrow(custom_datasets$four_clusters), nrow(four_clusters))
  expect_equal(ncol(custom_datasets$four_clusters), ncol(four_clusters))
})

test_that("package functions work with different data types", {
  datasets <- load_custom_datasets()
  
  for(dataset_name in names(datasets)) {
    dataset <- datasets[[dataset_name]]
    expect_s3_class(dataset, "data.frame")
    expect_gt(nrow(dataset), 0)
    expect_gt(ncol(dataset), 0)
    
    numeric_cols <- sapply(dataset, is.numeric)
    expect_gt(sum(numeric_cols), 0)
  }
})

test_that("UI components integrate with data loading", {
  ui <- nldr_viz_ui()
  ui_html <- as.character(ui)
  
  expect_true(grepl("four_clusters", ui_html))
  expect_true(grepl("pdfsense", ui_html))
  expect_true(grepl("trees", ui_html))
})

test_that("error handling works for invalid data", {
  # Test that load_custom_datasets handles invalid input gracefully
  datasets <- load_custom_datasets()
  expect_true(is.list(datasets))
  expect_gt(length(datasets), 0)
  
  # Test that accessing nonexistent names returns NULL
  expect_null(datasets[["nonexistent_dataset"]])
})

test_that("package metadata is consistent", {
  datasets <- load_custom_datasets()
  
  for(name in names(datasets)) {
    dataset <- datasets[[name]]
    expect_true(is.data.frame(dataset))
    expect_true(nrow(dataset) > 0)
    expect_true(ncol(dataset) > 0)
  }
})
