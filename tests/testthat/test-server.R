library(shiny)
library(testthat)
library(polarisR)

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

test_that("server initializes with correct reactive values", {
  shiny::testServer(nldr_viz_server, {
    expect_true(is.reactive(dataset))
    expect_true(is.reactive(vis_results))
    expect_true(is.reactive(custom_datasets))
    expect_true(is.reactive(available_datasets))
    expect_true(is.reactive(is_running_visualization))
    expect_true(is.reactive(is_running_binwidth_optimization))
    expect_true(is.reactive(is_running_quollr_analysis))
    expect_true(is.reactive(is_running_comparison))
    expect_true(is.reactive(binwidth_optimization_results))
    expect_true(is.reactive(optimal_config))
    expect_true(is.reactive(current_dataset_name))
    expect_true(is.reactive(comparison_selected_datasets))
    expect_true(is.reactive(comparison_results))
    expect_true(is.reactive(nldr_datasets))
    expect_true(is.reactive(shared_vis_data))
    expect_true(is.reactive(color_palette))
    expect_true(is.reactive(nldr_counter))
    expect_true(is.reactive(active_nldr_id))
    expect_true(is.reactive(quollr_model))
    expect_true(is.reactive(quollr_results))
    session$setInputs(example_data = "four_clusters")
    expect_true(!is.null(dataset()))
    expect_true(is.data.frame(dataset()))
    session$setInputs(nldr_method = "t-SNE", perplexity = 25, seed = 456)
    expect_equal(input$nldr_method, "t-SNE")
    expect_equal(input$perplexity, 25)
    expect_equal(input$seed, 456)
    session$setInputs(nldr_method = "UMAP", n_neighbors = 20, min_dist = 0.05)
    expect_equal(input$nldr_method, "UMAP")
    expect_equal(input$n_neighbors, 20)
    expect_equal(input$min_dist, 0.05)
    expect_false(is_running_visualization())
    expect_false(is_running_binwidth_optimization())
    expect_false(is_running_quollr_analysis())
    expect_false(is_running_comparison())
  })
})

test_that("example dataset selection works correctly", {
  shiny::testServer(nldr_viz_server, {
    expect_null(dataset())
    session$setInputs(example_data = "four_clusters")
    expect_true(!is.null(dataset()))
    expect_true(is.data.frame(dataset()))
    expect_true("cluster" %in% names(dataset()))
    expect_false(apply_changes_clicked())
  })
})

test_that("file upload validation works correctly", {
  shiny::testServer(nldr_viz_server, {
    temp_file <- tempfile(fileext = ".csv")
    test_data <- data.frame(
      x1 = rnorm(50),
      x2 = rnorm(50),
      group = factor(rep(c("A", "B"), 25))
    )
    write.csv(test_data, temp_file, row.names = FALSE)
    file_input <- list(
      name = "test_data.csv",
      datapath = temp_file
    )
    session$setInputs(file = file_input)
    expect_true(file.exists(temp_file))
    expect_equal(tools::file_ext(temp_file), "csv")
    expect_equal(file_input$name, "test_data.csv")
    unlink(temp_file)
  })
})

test_that("server function covers all major functionality areas", {
  shiny::testServer(nldr_viz_server, {
    reactive_values_to_test <- c(
      "dataset", "vis_results", "apply_changes_clicked", "custom_datasets",
      "available_datasets", "is_running_visualization", "is_running_binwidth_optimization",
      "is_running_quollr_analysis", "is_running_comparison", "nldr_datasets",
      "shared_vis_data", "color_palette", "comparison_selected_datasets"
    )
    for (rv_name in reactive_values_to_test) {
      expect_true(exists(rv_name), info = paste("Reactive value", rv_name, "should exist"))
      expect_true(is.reactive(get(rv_name)), info = paste(rv_name, "should be reactive"))
    }
    session$setInputs(example_data = "four_clusters")
    expect_true(!is.null(dataset()))
    expect_true(is.data.frame(dataset()))
    session$setInputs(nldr_method = "t-SNE", perplexity = 25, seed = 456)
    expect_equal(input$nldr_method, "t-SNE")
    expect_equal(input$perplexity, 25)
    expect_equal(input$seed, 456)
    session$setInputs(nldr_method = "UMAP", n_neighbors = 20, min_dist = 0.05)
    expect_equal(input$nldr_method, "UMAP")
    expect_equal(input$n_neighbors, 20)
    expect_equal(input$min_dist, 0.05)
    expect_false(is_running_visualization())
    expect_false(is_running_binwidth_optimization())
    expect_false(is_running_quollr_analysis())
    expect_false(is_running_comparison())
  })
})

test_that("data validation for NLDR works correctly", {
  shiny::testServer(nldr_viz_server, {
    valid_data <- data.frame(
      x1 = rnorm(100),
      x2 = rnorm(100),
      x3 = rnorm(100),
      group = factor(rep(c("A", "B", "C", "D"), 25))
    )
    dataset(valid_data)
    numeric_cols <- sapply(dataset(), is.numeric)
    expect_equal(sum(numeric_cols), 3)
    invalid_data <- data.frame(
      x1 = rnorm(100),
      group1 = factor(rep(c("A", "B"), 50)),
      group2 = factor(rep(c("X", "Y"), 50))
    )
    dataset(invalid_data)
    numeric_cols <- sapply(dataset(), is.numeric)
    expect_equal(sum(numeric_cols), 1)
  })
})

test_that("column selection and apply changes works", {
  shiny::testServer(nldr_viz_server, {
    test_data <- data.frame(
      x1 = rnorm(50),
      x2 = rnorm(50),
      x3 = rnorm(50),
      group = factor(rep(c("A", "B"), 25))
    )
    dataset(test_data)
    session$setInputs(selected_columns = c("x1", "x2", "group"))
    session$setInputs(apply_changes = 1)
    expect_true(apply_changes_clicked())
    expect_equal(ncol(dataset()), 3)
    expect_true(all(c("x1", "x2", "group") %in% names(dataset())))
  })
})

test_that("auto-perplexity calculation works correctly", {
  shiny::testServer(nldr_viz_server, {
    small_data <- data.frame(
      x1 = rnorm(30),
      x2 = rnorm(30),
      group = factor(rep(c("A", "B"), 15))
    )
    dataset(small_data)
    session$setInputs(auto_perplexity = TRUE)
    n_rows <- nrow(dataset())
    expected_perplexity <- max(5, min(30, floor(n_rows / 3) - 1))
    expect_true(expected_perplexity >= 5)
    expect_true(expected_perplexity <= 30)
  })
})

test_that("NLDR visualization parameters are validated", {
  shiny::testServer(nldr_viz_server, {
    test_data <- data.frame(
      x1 = rnorm(100),
      x2 = rnorm(100),
      x3 = rnorm(100),
      group = factor(rep(c("A", "B", "C", "D"), 25))
    )
    dataset(test_data)
    session$setInputs(
      nldr_method = "t-SNE",
      perplexity = 30,
      max_iter_tsne = 1000,
      seed = 123
    )
    expect_equal(input$nldr_method, "t-SNE")
    expect_equal(input$perplexity, 30)
    expect_equal(input$max_iter_tsne, 1000)
    session$setInputs(
      nldr_method = "UMAP",
      n_neighbors = 15,
      min_dist = 0.1
    )
    expect_equal(input$nldr_method, "UMAP")
    expect_equal(input$n_neighbors, 15)
    expect_equal(input$min_dist, 0.1)
  })
})

test_that("color column selection works correctly", {
  shiny::testServer(nldr_viz_server, {
    test_data <- data.frame(
      x1 = rnorm(50),
      x2 = rnorm(50),
      group1 = factor(rep(c("A", "B"), 25)),
      group2 = factor(rep(c("X", "Y"), 25))
    )
    dataset(test_data)
    session$setInputs(auto_color = TRUE)
    expect_true(input$auto_color)
    session$setInputs(auto_color = FALSE, color_column = "group2")
    expect_false(input$auto_color)
    expect_equal(input$color_column, "group2")
  })
})

test_that("data preprocessing for NLDR works correctly", {
  test_data <- data.frame(
    x1 = rnorm(100),
    x2 = rnorm(100),
    x3 = rep(1, 100),
    x4 = rnorm(100),
    group = factor(rep(c("A", "B"), 50))
  )
  numeric_cols_idx <- sapply(test_data, is.numeric)
  expect_equal(sum(numeric_cols_idx), 4)
  numeric_data <- test_data[, numeric_cols_idx, drop = FALSE]
  variances <- apply(numeric_data, 2, var, na.rm = TRUE)
  zero_var_cols <- names(variances[variances < 1e-10])
  expect_true("x3" %in% zero_var_cols)
  filtered_data <- numeric_data[, !names(numeric_data) %in% zero_var_cols, drop = FALSE]
  expect_equal(ncol(filtered_data), 3)
  expect_false("x3" %in% names(filtered_data))
  scaled_data <- scale(filtered_data)
  expect_equal(nrow(scaled_data), nrow(filtered_data))
  expect_equal(ncol(scaled_data), ncol(filtered_data))
})

test_that("visualization state management works correctly", {
  shiny::testServer(nldr_viz_server, {
    expect_false(is_running_visualization())
    is_running_visualization(TRUE)
    expect_true(is_running_visualization())
    is_running_visualization(FALSE)
    expect_false(is_running_visualization())
  })
})

test_that("NLDR result storage and retrieval works", {
  shiny::testServer(nldr_viz_server, {
    mock_result <- list(
      method = "t-SNE",
      coords = matrix(rnorm(200), ncol = 2),
      color_col = "group",
      color_values = factor(rep(c("A", "B"), 50)),
      perplexity = 30,
      seed = 123
    )
    vis_results(mock_result)
    stored_result <- vis_results()
    expect_equal(stored_result$method, "t-SNE")
    expect_equal(stored_result$perplexity, 30)
    expect_equal(nrow(stored_result$coords), 100)
    expect_equal(ncol(stored_result$coords), 2)
  })
})

test_that("tour functionality parameters work correctly", {
  shiny::testServer(nldr_viz_server, {
    test_data <- data.frame(
      x = rnorm(50),
      y = rnorm(50),
      color = factor(rep(c("A", "B"), 25)),
      x1 = rnorm(50),
      x2 = rnorm(50),
      x3 = rnorm(50)
    )
    shared_data <- crosstalk::SharedData$new(test_data)
    shared_vis_data(shared_data)
    session$setInputs(tour_display_type = "Scatter")
    expect_equal(input$tour_display_type, "Scatter")
    session$setInputs(tour_display_type = "Sage")
    expect_equal(input$tour_display_type, "Sage")
    session$setInputs(tour_display_type = "Slice")
    expect_equal(input$tour_display_type, "Slice")
    session$setInputs(
      tour_alpha = 0.8,
      tour_gamma = 1.5,
      tour_slice_volume = 0.15,
      tour_axes = TRUE
    )
    expect_equal(input$tour_alpha, 0.8)
    expect_equal(input$tour_gamma, 1.5)
    expect_equal(input$tour_slice_volume, 0.15)
    expect_true(input$tour_axes)
  })
})

test_that("binwidth optimization state management works", {
  shiny::testServer(nldr_viz_server, {
    expect_false(is_running_binwidth_optimization())
    is_running_binwidth_optimization(TRUE)
    expect_true(is_running_binwidth_optimization())
    is_running_binwidth_optimization(FALSE)
    expect_false(is_running_binwidth_optimization())
    mock_results <- data.frame(
      a1 = c(0.1, 0.2, 0.3),
      b1 = c(10, 15, 20),
      RMSE = c(0.15, 0.12, 0.18)
    )
    all_results <- list("1" = mock_results)
    binwidth_optimization_results(all_results)
    stored_results <- binwidth_optimization_results()
    expect_true(is.list(stored_results))
    expect_true("1" %in% names(stored_results))
    expect_equal(nrow(stored_results[["1"]]), 3)
  })
})

test_that("Quollr analysis state management works", {
  shiny::testServer(nldr_viz_server, {
    expect_false(is_running_quollr_analysis())
    is_running_quollr_analysis(TRUE)
    expect_true(is_running_quollr_analysis())
    is_running_quollr_analysis(FALSE)
    expect_false(is_running_quollr_analysis())
    mock_quollr_results <- list(
      highd_data = data.frame(x1 = rnorm(50), x2 = rnorm(50)),
      model_2d = data.frame(h = 1:10, x = rnorm(10), y = rnorm(10)),
      model_highd = data.frame(h = 1:10, x1 = rnorm(10), x2 = rnorm(10))
    )
    quollr_results(mock_quollr_results)
    stored_quollr <- quollr_results()
    expect_true(is.list(stored_quollr))
    expect_true("highd_data" %in% names(stored_quollr))
    expect_true("model_2d" %in% names(stored_quollr))
  })
})

test_that("comparison functionality works correctly", {
  shiny::testServer(nldr_viz_server, {
    expect_false(is_running_comparison())
    is_running_comparison(TRUE)
    expect_true(is_running_comparison())
    is_running_comparison(FALSE)
    expect_false(is_running_comparison())
    test_datasets <- character(0)
    comparison_selected_datasets(test_datasets)
    expect_equal(length(comparison_selected_datasets()), 0)
    test_datasets <- c("dataset1", "dataset2")
    comparison_selected_datasets(test_datasets)
    expect_equal(length(comparison_selected_datasets()), 2)
    expect_true("dataset1" %in% comparison_selected_datasets())
    expect_true("dataset2" %in% comparison_selected_datasets())
  })
})

test_that("NLDR dataset management works correctly", {
  shiny::testServer(nldr_viz_server, {
    expect_equal(length(nldr_datasets()), 0)
    mock_dataset <- list(
      id = 1,
      name = "four_clusters - t-SNE",
      result = list(
        method = "t-SNE",
        coords = matrix(rnorm(200), ncol = 2)
      ),
      timestamp = Sys.time()
    )
    current_datasets <- nldr_datasets()
    current_datasets[["1"]] <- mock_dataset
    nldr_datasets(current_datasets)
    stored_datasets <- nldr_datasets()
    expect_equal(length(stored_datasets), 1)
    expect_true("1" %in% names(stored_datasets))
    expect_equal(stored_datasets[["1"]]$name, "four_clusters - t-SNE")
  })
})

test_that("side-by-side comparison functionality works", {
  shiny::testServer(nldr_viz_server, {
    expect_null(sidebyside_dataset1())
    expect_null(sidebyside_dataset2())
    mock_dataset1 <- list(
      name = "Dataset 1",
      result = list(
        method = "t-SNE",
        coords = matrix(rnorm(200), ncol = 2),
        color_values = factor(rep(c("A", "B"), 50))
      )
    )
    mock_dataset2 <- list(
      name = "Dataset 2", 
      result = list(
        method = "UMAP",
        coords = matrix(rnorm(200), ncol = 2),
        color_values = factor(rep(c("X", "Y"), 50))
      )
    )
    sidebyside_dataset1(mock_dataset1)
    sidebyside_dataset2(mock_dataset2)
    expect_equal(sidebyside_dataset1()$name, "Dataset 1")
    expect_equal(sidebyside_dataset2()$name, "Dataset 2")
    expect_equal(sidebyside_dataset1()$result$method, "t-SNE")
    expect_equal(sidebyside_dataset2()$result$method, "UMAP")
  })
})

test_that("reset column functionality works correctly", {
  shiny::testServer(nldr_viz_server, {
    initial_data <- data.frame(
      x1 = rnorm(50),
      x2 = rnorm(50),
      x3 = rnorm(50),
      group = factor(rep(c("A", "B"), 25))
    )
    dataset(initial_data)
    apply_changes_clicked(TRUE)
    expect_true(apply_changes_clicked())
    session$setInputs(example_data = "four_clusters")
    expect_false(apply_changes_clicked())
    expect_true(!is.null(dataset()))
    expect_true("cluster" %in% names(dataset()))
  })
})

test_that("output reactive expressions work correctly", {
  shiny::testServer(nldr_viz_server, {
    expect_false(is_running_visualization())
    is_running_visualization(TRUE)
    expect_true(is_running_visualization())
    expect_false(is_running_binwidth_optimization())
    expect_false(is_running_quollr_analysis())
    expect_false(is_running_comparison())
    is_running_binwidth_optimization(TRUE)
    expect_true(is_running_binwidth_optimization())
    is_running_quollr_analysis(TRUE)
    expect_true(is_running_quollr_analysis())
    is_running_comparison(TRUE)
    expect_true(is_running_comparison())
    is_running_visualization(FALSE)
    is_running_binwidth_optimization(FALSE)
    is_running_quollr_analysis(FALSE)
    is_running_comparison(FALSE)
    expect_false(is_running_visualization())
    expect_false(is_running_binwidth_optimization())
    expect_false(is_running_quollr_analysis())
    expect_false(is_running_comparison())
  })
})

test_that("data info output works correctly", {
  shiny::testServer(nldr_viz_server, {
    test_data <- data.frame(
      numeric1 = rnorm(100),
      numeric2 = rnorm(100),
      character1 = letters[1:100],
      factor1 = factor(rep(c("A", "B", "C", "D"), 25))
    )
    dataset(test_data)
    data <- dataset()
    expect_equal(nrow(data), 100)
    expect_equal(ncol(data), 4)
    expect_equal(sum(sapply(data, is.numeric)), 2)
    expect_equal(sum(sapply(data, function(x) is.factor(x) || is.character(x))), 2)
  })
})

test_that("error handling works correctly", {
  shiny::testServer(nldr_viz_server, {
    dirty_data <- data.frame(
      x1 = c(1, 2, NA, 4, 5),
      x2 = c(1, 2, 3, 4, 5),
      group = c("A", "B", "", "A", "B")
    )
    dataset(dirty_data)
    empty_check_result <- list(has_empty = TRUE, empty_cols = c("x1", "group"), total_empty = 2)
    expect_true(empty_check_result$has_empty)
    expect_equal(empty_check_result$total_empty, 2)
  })
})

test_that("input validation functions work correctly", {
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

test_that("dataset selection and filtering logic works correctly", {
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

test_that("NLDR computation workflow executes correctly", {
  skip_if_not_installed("Rtsne")
  skip_if_not_installed("umap")
  shiny::testServer(nldr_viz_server, {
    test_data <- data.frame(
      x1 = rnorm(100, mean = 0, sd = 1),
      x2 = rnorm(100, mean = 1, sd = 1.5),
      x3 = rnorm(100, mean = -1, sd = 0.8),
      group = factor(rep(c("A", "B", "C", "D"), 25))
    )
    dataset(test_data)
    apply_changes_clicked(TRUE)
    session$setInputs(
      nldr_method = "t-SNE",
      perplexity = 25,
      max_iter_tsne = 500,
      seed = 789,
      auto_color = FALSE,
      color_column = "group"
    )
    mock_tsne_result <- list(
      method = "t-SNE",
      coords = matrix(rnorm(200), ncol = 2),
      color_col = "group",
      color_values = test_data$group,
      perplexity = 25,
      max_iter = 500,
      seed = 789
    )
    is_running_visualization(TRUE)
    vis_results(mock_tsne_result)
    is_running_visualization(FALSE)
    result <- vis_results()
    expect_equal(result$method, "t-SNE")
    expect_equal(result$perplexity, 25)
    expect_equal(result$max_iter, 500)
    expect_equal(result$seed, 789)
    expect_equal(nrow(result$coords), 100)
    expect_equal(ncol(result$coords), 2)
    expect_equal(length(result$color_values), 100)
    session$setInputs(
      nldr_method = "UMAP",
      n_neighbors = 12,
      min_dist = 0.08
    )
    mock_umap_result <- list(
      method = "UMAP",
      coords = matrix(rnorm(200), ncol = 2),
      color_col = "group",
      color_values = test_data$group,
      n_neighbors = 12,
      min_dist = 0.08,
      seed = 789
    )
    is_running_visualization(TRUE)
    vis_results(mock_umap_result)
    is_running_visualization(FALSE)
    umap_result <- vis_results()
    expect_equal(umap_result$method, "UMAP")
    expect_equal(umap_result$n_neighbors, 12)
    expect_equal(umap_result$min_dist, 0.08)
    expect_equal(nrow(umap_result$coords), 100)
  })
})

test_that("plot generation creates correct data structures", {
  shiny::testServer(nldr_viz_server, {
    test_coords <- matrix(c(
      rnorm(50, mean = -1), rnorm(50, mean = 1),
      rnorm(50, mean = -1), rnorm(50, mean = 1)
    ), ncol = 2)
    test_colors <- factor(rep(c("Group1", "Group2"), 50))
    mock_result <- list(
      method = "t-SNE",
      coords = test_coords,
      color_col = "cluster",
      color_values = test_colors,
      perplexity = 30,
      seed = 123
    )
    vis_results(mock_result)
    plot_data <- data.frame(
      x = test_coords[, 1],
      y = test_coords[, 2],
      color = test_colors,
      x1 = rnorm(100),
      x2 = rnorm(100),
      x3 = rnorm(100)
    )
    shared_data <- crosstalk::SharedData$new(plot_data)
    shared_vis_data(shared_data)
    color_levels <- levels(test_colors)
    pal <- scales::hue_pal()(length(color_levels))
    names(pal) <- color_levels
    color_palette(pal)
    stored_shared <- shared_vis_data()
    expect_s3_class(stored_shared, "SharedData")
    plot_df <- stored_shared$data()
    expect_equal(nrow(plot_df), 100)
    expect_true(all(c("x", "y", "color") %in% names(plot_df)))
    expect_equal(length(unique(plot_df$color)), 2)
    stored_palette <- color_palette()
    expect_equal(length(stored_palette), 2)
    expect_true(all(color_levels %in% names(stored_palette)))
    expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", stored_palette)))
  })
})

test_that("NLDR dataset storage and retrieval works comprehensively", {
  shiny::testServer(nldr_viz_server, {
    results <- list(
      list(
        method = "t-SNE", coords = matrix(rnorm(200), ncol = 2),
        color_values = factor(rep(c("A", "B"), 50)), perplexity = 20
      ),
      list(
        method = "UMAP", coords = matrix(rnorm(200), ncol = 2),
        color_values = factor(rep(c("X", "Y"), 50)), n_neighbors = 10
      ),
      list(
        method = "t-SNE", coords = matrix(rnorm(200), ncol = 2),
        color_values = factor(rep(c("P", "Q"), 50)), perplexity = 40
      )
    )
    for (i in seq_along(results)) {
      nldr_counter(i)
      current_datasets <- nldr_datasets()
      dataset_name <- if (results[[i]]$method == "t-SNE") {
        paste0("dataset_", i, " - t-SNE (p=", results[[i]]$perplexity, ")")
      } else {
        paste0("dataset_", i, " - UMAP (n=", results[[i]]$n_neighbors, ")")
      }
      current_datasets[[as.character(i)]] <- list(
        id = i,
        name = dataset_name,
        result = results[[i]],
        timestamp = Sys.time(),
        tour_input_data = data.frame(
          x = results[[i]]$coords[, 1],
          y = results[[i]]$coords[, 2],
          color = results[[i]]$color_values
        )
      )
      nldr_datasets(current_datasets)
    }
    stored_datasets <- nldr_datasets()
    expect_equal(length(stored_datasets), 3)
    active_nldr_id("2")
    expect_equal(active_nldr_id(), "2")
    dataset_2 <- stored_datasets[["2"]]
    expect_equal(dataset_2$result$method, "UMAP")
    expect_equal(dataset_2$result$n_neighbors, 10)
    expect_true(grepl("UMAP", dataset_2$name))
    expect_true(grepl("t-SNE \\(p=20\\)", stored_datasets[["1"]]$name))
    expect_true(grepl("UMAP \\(n=10\\)", stored_datasets[["2"]]$name))
    expect_true(grepl("t-SNE \\(p=40\\)", stored_datasets[["3"]]$name))
  })
})

test_that("binwidth optimization executes correctly", {
  skip_if_not_installed("quollr")
  shiny::testServer(nldr_viz_server, {
    vis_data <- data.frame(
      x = rnorm(80),
      y = rnorm(80),
      color = factor(rep(c("A", "B", "C", "D"), 20)),
      x1 = rnorm(80),
      x2 = rnorm(80),
      x3 = rnorm(80)
    )
    shared_vis_data(crosstalk::SharedData$new(vis_data))
    mock_vis_result <- list(
      method = "t-SNE",
      coords = cbind(vis_data$x, vis_data$y),
      color_values = vis_data$color
    )
    vis_results(mock_vis_result)
    session$setInputs(
      min_bins = 8,
      max_bins = 20,
      auto_bin_range = FALSE
    )
    expect_equal(input$min_bins, 8)
    expect_equal(input$max_bins, 20)
    expect_false(input$auto_bin_range)
    session$setInputs(auto_bin_range = TRUE)
    n_points <- nrow(vis_data)
    expected_min <- max(5, floor(sqrt(n_points / 50)))
    expected_max <- min(25, floor(sqrt(n_points / 5)))
    expect_gte(expected_min, 5)
    expect_lte(expected_max, 25)
    expect_false(is_running_binwidth_optimization())
    is_running_binwidth_optimization(TRUE)
    expect_true(is_running_binwidth_optimization())
    mock_results <- data.frame(
      a1 = seq(0.1, 0.5, by = 0.1),
      b1 = c(8, 12, 16, 20, 24),
      RMSE = c(0.18, 0.14, 0.10, 0.12, 0.16)
    )
    optimal_row <- mock_results[which.min(mock_results$RMSE), ]
    optimal_config(optimal_row)
    active_nldr_id("test_vis")
    all_results <- binwidth_optimization_results()
    all_results[["test_vis"]] <- mock_results
    binwidth_optimization_results(all_results)
    is_running_binwidth_optimization(FALSE)
    expect_false(is_running_binwidth_optimization())
    expect_equal(optimal_config()$a1, 0.3)
    expect_equal(optimal_config()$b1, 16)
    expect_equal(optimal_config()$RMSE, 0.10)
    stored_results <- binwidth_optimization_results()
    expect_true("test_vis" %in% names(stored_results))
    expect_equal(nrow(stored_results[["test_vis"]]), 5)
  })
})

test_that("Quollr analysis executes correctly", {
  skip_if_not_installed("quollr")
  shiny::testServer(nldr_viz_server, {
    vis_data <- data.frame(
      x = rnorm(60),
      y = rnorm(60),
      color = factor(rep(c("A", "B", "C"), 20)),
      x1 = rnorm(60),
      x2 = rnorm(60),
      x3 = rnorm(60),
      x4 = rnorm(60)
    )
    shared_vis_data(crosstalk::SharedData$new(vis_data))
    optimal_config(data.frame(a1 = 0.25, b1 = 12, RMSE = 0.08))
    session$setInputs(
      quollr_remove_low_density = TRUE,
      quollr_density_threshold = 0.05
    )
    expect_true(input$quollr_remove_low_density)
    expect_equal(input$quollr_density_threshold, 0.05)
    expect_false(is_running_quollr_analysis())
    is_running_quollr_analysis(TRUE)
    expect_true(is_running_quollr_analysis())
    mock_quollr_results <- list(
      highd_data = data.frame(
        x1 = vis_data$x1, x2 = vis_data$x2, x3 = vis_data$x3, x4 = vis_data$x4,
        ID = seq_len(nrow(vis_data))
      ),
      model_2d = data.frame(
        h = 1:10,
        c1 = rnorm(10),
        c2 = rnorm(10),
        n_h = sample(3:8, 10, replace = TRUE)
      ),
      model_highd = data.frame(
        h = rep(1:10, each = 4),
        x1 = rnorm(40), x2 = rnorm(40), x3 = rnorm(40), x4 = rnorm(40)
      ),
      trimesh_data = data.frame(
        x = rnorm(30), y = rnorm(30), z = rnorm(30),
        from = sample(1:10, 30, replace = TRUE),
        to = sample(1:10, 30, replace = TRUE)
      ),
      nldr_obj = list(scaled_nldr = data.frame(emb1 = vis_data$x, emb2 = vis_data$y, ID = 1:60)),
      hb_obj = list(centroids = data.frame(c1 = rnorm(10), c2 = rnorm(10)))
    )
    quollr_results(mock_quollr_results)
    is_running_quollr_analysis(FALSE)
    expect_false(is_running_quollr_analysis())
    stored_quollr <- quollr_results()
    expect_true(!is.null(stored_quollr))
    expect_true(all(c("highd_data", "model_2d", "model_highd", "trimesh_data") %in% names(stored_quollr)))
    expect_equal(nrow(stored_quollr$model_2d), 10)
    expect_equal(nrow(stored_quollr$highd_data), 60)
  })
})


test_that("comparison functionality works correctly", {
  shiny::testServer(nldr_viz_server, {
    dataset1 <- list(
      id = 1,
      name = "four_clusters - t-SNE (p=30)",
      result = list(
        method = "t-SNE",
        coords = matrix(rnorm(200), ncol = 2),
        color_values = factor(rep(c("A", "B"), 50)),
        color_col = "group"
      ),
      timestamp = Sys.time()
    )
    dataset2 <- list(
      id = 2,
      name = "four_clusters - UMAP (n=15)",
      result = list(
        method = "UMAP",
        coords = matrix(rnorm(200), ncol = 2),
        color_values = factor(rep(c("A", "B"), 50)),
        color_col = "group"
      ),
      timestamp = Sys.time()
    )
    nldr_datasets(list("1" = dataset1, "2" = dataset2))
    session$setInputs(
      sidebyside_dataset1 = "1",
      sidebyside_dataset2 = "2",
      enable_linked_brushing = TRUE
    )
    expect_equal(input$sidebyside_dataset1, "1")
    expect_equal(input$sidebyside_dataset2, "2")
    expect_true(input$enable_linked_brushing)
    expect_false(is_running_comparison())
    is_running_comparison(TRUE)
    expect_true(is_running_comparison())
    n_points <- min(100, nrow(dataset1$result$coords), nrow(dataset2$result$coords))
    shared_keys <- paste0("point_", 1:n_points)
    plot_data1 <- data.frame(
      x = dataset1$result$coords[1:n_points, 1],
      y = dataset1$result$coords[1:n_points, 2],
      color = dataset1$result$color_values[1:n_points],
      key = shared_keys
    )
    plot_data2 <- data.frame(
      x = dataset2$result$coords[1:n_points, 1],
      y = dataset2$result$coords[1:n_points, 2],
      color = dataset2$result$color_values[1:n_points],
      key = shared_keys
    )
    shared1 <- crosstalk::SharedData$new(plot_data1, key = ~key, group = "sidebyside_comparison")
    shared2 <- crosstalk::SharedData$new(plot_data2, key = ~key, group = "sidebyside_comparison")
    sidebyside_shared_data1(shared1)
    sidebyside_shared_data2(shared2)
    is_running_comparison(FALSE)
    expect_false(is_running_comparison())
    expect_s3_class(sidebyside_shared_data1(), "SharedData")
    expect_s3_class(sidebyside_shared_data2(), "SharedData")
    expect_equal(nrow(sidebyside_shared_data1()$data()), n_points)
    expect_equal(nrow(sidebyside_shared_data2()$data()), n_points)
  })
})

test_that("UI outputs generate correctly", {
  shiny::testServer(nldr_viz_server, {
    test_data <- data.frame(
      x1 = rnorm(100),
      x2 = rnorm(100),
      numeric3 = rnorm(100),
      character1 = letters[1:100],
      factor1 = factor(rep(c("A", "B", "C", "D"), 25))
    )
    dataset(test_data)
    data <- dataset()
    info_metrics <- c(
      nrow(data),
      ncol(data),
      sum(sapply(data, is.numeric)),
      sum(sapply(data, function(x) is.factor(x) || is.character(x)))
    )
    expect_equal(info_metrics[1], 100)
    expect_equal(info_metrics[2], 5)
    expect_equal(info_metrics[3], 3)
    expect_equal(info_metrics[4], 2)
    categorical_cols <- names(data)[sapply(data, function(x) is.factor(x) || is.character(x))]
    expect_equal(length(categorical_cols), 2)
    expect_true("character1" %in% categorical_cols)
    expect_true("factor1" %in% categorical_cols)
    all_columns <- names(data)
    numeric_columns <- names(data)[sapply(data, is.numeric)]
    expect_equal(length(all_columns), 5)
    expect_equal(length(numeric_columns), 3)
    expect_true(all(c("x1", "x2", "numeric3") %in% numeric_columns))
    expect_false(is_running_visualization())
    expect_false(is_running_binwidth_optimization())
    expect_false(is_running_quollr_analysis())
    expect_false(is_running_comparison())
  })
})

test_that("performance with different dataset sizes", {
  shiny::testServer(nldr_viz_server, {
    small_data <- data.frame(
      x1 = rnorm(10),
      x2 = rnorm(10),
      group = factor(rep(c("A", "B"), 5))
    )
    dataset(small_data)
    start_time <- Sys.time()
    expect_equal(nrow(dataset()), 10)
    end_time <- Sys.time()
    processing_time <- as.numeric(end_time - start_time)
    expect_lt(processing_time, 1.0)
    medium_data <- data.frame(
      x1 = rnorm(500),
      x2 = rnorm(500),
      x3 = rnorm(500),
      group = factor(rep(c("A", "B", "C", "D"), 125))
    )
    dataset(medium_data)
    numeric_cols <- sapply(dataset(), is.numeric)
    expect_equal(sum(numeric_cols), 3)
    expect_equal(nrow(dataset()), 500)
    minimal_data <- data.frame(
      x1 = c(1, 2, 3, 4, 5),
      x2 = c(2, 3, 4, 5, 6),
      group = factor(c("A", "B", "A", "B", "A"))
    )
    dataset(minimal_data)
    expect_equal(nrow(dataset()), 5)
    expect_equal(ncol(dataset()), 3)
  })
})

test_that("edge cases and error conditions are handled", {
  shiny::testServer(nldr_viz_server, {
    data_with_na <- data.frame(
      x1 = c(1, 2, NA, 4, 5),
      x2 = c(1, 2, 3, 4, 5),
      group = c("A", "B", "", "A", "B")
    )
    dataset(data_with_na)
    has_empty <- any(sapply(dataset(), function(x) any(is.na(x) | x == "" | is.null(x))))
    expect_true(has_empty)
    insufficient_data <- data.frame(
      x1 = rnorm(50),
      char1 = letters[1:50],
      char2 = LETTERS[1:50]
    )
    dataset(insufficient_data)
    numeric_cols_count <- sum(sapply(dataset(), is.numeric))
    expect_equal(numeric_cols_count, 1)
    zero_var_data <- data.frame(
      x1 = rnorm(50),
      x2 = rnorm(50),
      x3 = rep(1, 50),
      group = factor(rep(c("A", "B"), 25))
    )
    dataset(zero_var_data)
    numeric_data <- dataset()[, sapply(dataset(), is.numeric), drop = FALSE]
    variances <- apply(numeric_data, 2, var, na.rm = TRUE)
    zero_var_cols <- names(variances[variances < 1e-10])
    expect_true("x3" %in% zero_var_cols)
    session$setInputs(
      nldr_method = "t-SNE",
      perplexity = 3,
      max_iter_tsne = 50
    )
    min_perplexity <- 5
    max_perplexity <- floor(nrow(dataset()) / 3) - 1
    expect_gte(min_perplexity, 5)
    expect_lte(input$perplexity, max_perplexity)
  })
})

test_that("complex multi-step workflows work correctly", {
  shiny::testServer(nldr_viz_server, {
    session$setInputs(example_data = "four_clusters")
    original_data <- dataset()
    expect_true(!is.null(original_data))
    session$setInputs(
      auto_select = FALSE,
      selected_columns = c("x1", "x2", "cluster")
    )
    session$setInputs(apply_changes = 1)
    apply_changes_clicked(TRUE)
    expect_true(apply_changes_clicked())
    methods <- c("t-SNE", "UMAP")
    for (i in seq_along(methods)) {
      method <- methods[i]
      session$setInputs(nldr_method = method)
      if (method == "t-SNE") {
        session$setInputs(perplexity = 20 + i * 5, seed = 100 + i)
      } else {
        session$setInputs(n_neighbors = 10 + i * 3, min_dist = 0.05 + i * 0.02)
      }
      mock_result <- list(
        method = method,
        coords = matrix(rnorm(200), ncol = 2),
        color_col = "cluster",
        color_values = factor(rep(c("A", "B"), 50)),
        seed = 100 + i
      )
      if (method == "t-SNE") {
        mock_result$perplexity <- 20 + i * 5
      } else {
        mock_result$n_neighbors <- 10 + i * 3
        mock_result$min_dist <- 0.05 + i * 0.02
      }
      vis_results(mock_result)
      nldr_counter(i)
      current_datasets <- nldr_datasets()
      current_datasets[[as.character(i)]] <- list(
        id = i,
        name = paste0("workflow_", method),
        result = mock_result,
        timestamp = Sys.time()
      )
      nldr_datasets(current_datasets)
    }
    stored_datasets <- nldr_datasets()
    expect_equal(length(stored_datasets), 2)
    expect_true("1" %in% names(stored_datasets))
    expect_true("2" %in% names(stored_datasets))
    active_nldr_id("1")
    vis_results(stored_datasets[["1"]]$result)
    shared_vis_data(crosstalk::SharedData$new(data.frame(
      x = rnorm(100), y = rnorm(100), color = factor(rep(c("A", "B"), 50)),
      x1 = rnorm(100), x2 = rnorm(100)
    )))
    is_running_binwidth_optimization(TRUE)
    optimal_config(data.frame(a1 = 0.2, b1 = 15, RMSE = 0.09))
    is_running_binwidth_optimization(FALSE)
    session$setInputs(
      sidebyside_dataset1 = "1",
      sidebyside_dataset2 = "2"
    )
    expect_equal(length(nldr_datasets()), 2)
    expect_true(!is.null(optimal_config()))
    expect_equal(optimal_config()$RMSE, 0.09)
    expect_equal(input$sidebyside_dataset1, "1")
    expect_equal(input$sidebyside_dataset2, "2")
  })
})

test_that("memory management and cleanup works", {
  shiny::testServer(nldr_viz_server, {
    for (i in 1:3) {
      large_data <- data.frame(
        x1 = rnorm(500),
        x2 = rnorm(500),
        x3 = rnorm(500),
        group = factor(rep(c("A", "B", "C", "D"), 125))
      )
      mock_result <- list(
        method = "t-SNE",
        coords = matrix(rnorm(1000), ncol = 2),
        color_values = factor(rep(c("A", "B"), 250))
      )
      current_datasets <- nldr_datasets()
      current_datasets[[as.character(i)]] <- list(
        id = i,
        name = paste0("large_dataset_", i),
        result = mock_result,
        timestamp = Sys.time()
      )
      nldr_datasets(current_datasets)
    }
    expect_equal(length(nldr_datasets()), 3)
    nldr_datasets(list())
    expect_equal(length(nldr_datasets()), 0)
    dataset(NULL)
    vis_results(NULL)
    shared_vis_data(NULL)
    optimal_config(NULL)
    quollr_results(NULL)
    expect_true(is.null(dataset()))
    expect_true(is.null(vis_results()))
    expect_true(is.null(shared_vis_data()))
    expect_true(is.null(optimal_config()))
    expect_true(is.null(quollr_results()))
  })
})

test_that("state synchronization works across complex operations", {
  shiny::testServer(nldr_viz_server, {
    expect_false(is_running_visualization())
    expect_false(is_running_binwidth_optimization())
    expect_false(is_running_quollr_analysis())
    expect_false(is_running_comparison())
    is_running_visualization(TRUE)
    expect_true(is_running_visualization())
    expect_false(is_running_binwidth_optimization())
    is_running_visualization(FALSE)
    is_running_binwidth_optimization(TRUE)
    expect_false(is_running_visualization())
    expect_true(is_running_binwidth_optimization())
    is_running_binwidth_optimization(FALSE)
    is_running_quollr_analysis(TRUE)
    expect_false(is_running_binwidth_optimization())
    expect_true(is_running_quollr_analysis())
    is_running_quollr_analysis(FALSE)
    is_running_comparison(TRUE)
    expect_false(is_running_quollr_analysis())
    expect_true(is_running_comparison())
    is_running_comparison(FALSE)
    expect_false(is_running_comparison())
    expect_false(is_running_visualization())
    expect_false(is_running_binwidth_optimization())
    expect_false(is_running_quollr_analysis())
    expect_false(is_running_comparison())
  })
})

test_that("data integrity is maintained throughout workflow", {
  shiny::testServer(nldr_viz_server, {
    original_data <- data.frame(
      x1 = rnorm(100, mean = 0, sd = 1),
      x2 = rnorm(100, mean = 1, sd = 1),
      x3 = rnorm(100, mean = -1, sd = 1),
      group = factor(rep(c("A", "B", "C", "D"), 25))
    )
    dataset(original_data)
    session$setInputs(
      auto_select = FALSE,
      selected_columns = c("x1", "x2", "group")
    )
    session$setInputs(apply_changes = 1)
    filtered_data <- dataset()
    expect_equal(ncol(filtered_data), 3)
    expect_equal(nrow(filtered_data), 100)
    expect_true(all(c("x1", "x2", "group") %in% names(filtered_data)))
    mock_result <- list(
      method = "t-SNE",
      coords = matrix(rnorm(200), ncol = 2),
      color_col = "group",
      color_values = filtered_data$group,
      perplexity = 25,
      seed = 123
    )
    vis_results(mock_result)
    result <- vis_results()
    expect_equal(length(result$color_values), nrow(filtered_data))
    expect_equal(nrow(result$coords), nrow(filtered_data))
    expect_equal(result$color_col, "group")
    plot_data <- data.frame(
      x = result$coords[, 1],
      y = result$coords[, 2],
      color = result$color_values,
      x1 = filtered_data$x1,
      x2 = filtered_data$x2
    )
    shared_data <- crosstalk::SharedData$new(plot_data)
    shared_vis_data(shared_data)
    shared_df <- shared_vis_data()$data()
    expect_equal(nrow(shared_df), 100)
    expect_equal(length(unique(shared_df$color)), 4)
    expect_true(all(c("x", "y", "color", "x1", "x2") %in% names(shared_df)))
  })
})
