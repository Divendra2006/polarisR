library(polarisR)

test_that("load_custom_datasets returns valid datasets", {
  datasets <- load_custom_datasets()
  
  expect_type(datasets, "list")
  
  expect_true("four_clusters" %in% names(datasets))
  expect_true("pdfsense" %in% names(datasets))
  expect_true("trees" %in% names(datasets))
  
  expect_s3_class(datasets$four_clusters, "data.frame")
  expect_s3_class(datasets$pdfsense, "data.frame")
  expect_s3_class(datasets$trees, "data.frame")
  
  expect_gt(nrow(datasets$four_clusters), 0)
  expect_gt(ncol(datasets$four_clusters), 0)
  expect_gt(nrow(datasets$pdfsense), 0)
  expect_gt(ncol(datasets$pdfsense), 0)
  expect_gt(nrow(datasets$trees), 0)
  expect_gt(ncol(datasets$trees), 0)
})

test_that("datasets have expected structure", {
  datasets <- load_custom_datasets()
  
  four_clusters <- datasets$four_clusters
  expect_true("x1" %in% names(four_clusters))
  expect_true("x2" %in% names(four_clusters))
  expect_true("cluster" %in% names(four_clusters))
  
  pdfsense <- datasets$pdfsense
  numeric_cols <- sapply(pdfsense, is.numeric)
  expect_gt(sum(numeric_cols), 1)
  
  trees <- datasets$trees
  expect_gt(ncol(trees), 1)
})

test_that("datasets don't contain missing values in key columns", {
  datasets <- load_custom_datasets()
  
  four_clusters <- datasets$four_clusters
  expect_false(any(is.na(four_clusters$x1)))
  expect_false(any(is.na(four_clusters$x2)))
  expect_false(any(is.na(four_clusters$cluster)))
  
  pdfsense <- datasets$pdfsense
  expect_true(is.data.frame(pdfsense))
  expect_gt(nrow(pdfsense), 0)
  
  trees <- datasets$trees
  expect_true(is.data.frame(trees))
  expect_gt(nrow(trees), 0)
})
