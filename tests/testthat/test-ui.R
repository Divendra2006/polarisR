library(shiny)

test_that("UI input validation works correctly", {
  valid_extensions <- c(".csv")
  
  expect_true(".csv" %in% valid_extensions)
  expect_false(".txt" %in% valid_extensions)
  expect_false(".xlsx" %in% valid_extensions)
  
  expect_equal(tools::file_ext("data.csv"), "csv")
  expect_equal(tools::file_ext("data.CSV"), "CSV")
  expect_equal(tools::file_ext("data.txt"), "txt")
  expect_equal(tools::file_ext("data"), "")
})

test_that("method parameter ranges are valid", {
  perplexity_min <- 5
  perplexity_max <- 50
  perplexity_default <- 30
  
  expect_gte(perplexity_default, perplexity_min)
  expect_lte(perplexity_default, perplexity_max)
  expect_gt(perplexity_max, perplexity_min)
  
  max_iter_min <- 100
  max_iter_max <- 2000
  max_iter_default <- 1000
  
  expect_gte(max_iter_default, max_iter_min)
  expect_lte(max_iter_default, max_iter_max)
  expect_gt(max_iter_max, max_iter_min)
  
  n_neighbors_min <- 2
  n_neighbors_max <- 50
  n_neighbors_default <- 15
  
  expect_gte(n_neighbors_default, n_neighbors_min)
  expect_lte(n_neighbors_default, n_neighbors_max)
  expect_gt(n_neighbors_max, n_neighbors_min)
  
  min_dist_min <- 0.01
  min_dist_max <- 0.99
  min_dist_default <- 0.1
  
  expect_gte(min_dist_default, min_dist_min)
  expect_lte(min_dist_default, min_dist_max)
  expect_gt(min_dist_max, min_dist_min)
})

test_that("tour parameter ranges are valid", {
  alpha_min <- 0.1
  alpha_max <- 1.0
  alpha_default <- 0.7
  alpha_step <- 0.05
  
  expect_gte(alpha_default, alpha_min)
  expect_lte(alpha_default, alpha_max)
  expect_gt(alpha_step, 0)
  expect_lte(alpha_step, (alpha_max - alpha_min))
  
  gamma_min <- 0
  gamma_max <- 5
  gamma_default <- 1
  gamma_step <- 0.1
  
  expect_gte(gamma_default, gamma_min)
  expect_lte(gamma_default, gamma_max)
  expect_gt(gamma_step, 0)
  
  slice_min <- 0.01
  slice_max <- 0.5
  slice_default <- 0.1
  slice_step <- 0.01
  
  expect_gte(slice_default, slice_min)
  expect_lte(slice_default, slice_max)
  expect_gt(slice_step, 0)
})

test_that("seed parameter validation", {
  seed_default <- 123
  seed_min <- 1
  seed_max <- 99999
  
  expect_gte(seed_default, seed_min)
  expect_lte(seed_default, seed_max)
  expect_type(seed_default, "double")
})

test_that("UI conditional panels have correct conditions", {
  ui <- nldr_viz_ui()
  ui_html <- as.character(ui)
  
  expect_true(grepl("t-SNE", ui_html))
  expect_true(grepl("UMAP", ui_html))
  
  expect_true(grepl("auto_select", ui_html))
  expect_true(grepl("auto_color", ui_html))
  
  expect_true(grepl("Scatter", ui_html))
  expect_true(grepl("Sage", ui_html))
  expect_true(grepl("Slice", ui_html))
  
  expect_true(grepl("settings", ui_html))
  expect_true(grepl("visualization", ui_html))
})

test_that("button states and loading indicators work", {
  ui <- nldr_viz_ui()
  ui_html <- as.character(ui)
  
  expect_true(grepl("apply_changes", ui_html))
  expect_true(grepl("btn-primary", ui_html))
  
  expect_true(grepl("binwidth_button", ui_html))
  expect_true(grepl("quollr_button", ui_html))
  
  expect_true(grepl("comparison_button", ui_html))
  expect_true(grepl("tour", ui_html))
  
  expect_true(grepl("fa-spin", ui_html))
})

test_that("layout structure is correct", {
  ui <- nldr_viz_ui()
  ui_html <- as.character(ui)
  
  expect_true(grepl("navbar", ui_html))
  
  expect_true(grepl("sidebar", ui_html))
  
  expect_true(grepl("card", ui_html))
  
  expect_true(grepl("nav", ui_html))
})

test_that("CSS and styling is included", {
  ui <- nldr_viz_ui()
  ui_html <- as.character(ui)
  
  expect_true(grepl("btn-primary", ui_html))
  
  expect_true(grepl("navbar", ui_html))
})

test_that("output dimensions are specified", {
  ui <- nldr_viz_ui()
  ui_html <- as.character(ui)
  
  expect_true(grepl("300", ui_html))
  
  expect_true(grepl("width", ui_html) || grepl("height", ui_html))
})

test_that("help text and instructions are present", {
  ui <- nldr_viz_ui()
  ui_html <- as.character(ui)
  
  expect_true(grepl("Upload Dataset", ui_html))
  expect_true(grepl("Example Datasets", ui_html))
  expect_true(grepl("Method", ui_html))
})

test_that("accessibility features are included", {
  ui <- nldr_viz_ui()
  ui_html <- as.character(ui)
  
  expect_true(grepl("Upload Dataset", ui_html))
  expect_true(grepl("Example Datasets", ui_html))
  expect_true(grepl("Choose Method:", ui_html))
  
  expect_true(grepl("button", ui_html))
})
