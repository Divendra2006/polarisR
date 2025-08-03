#' NLDR Visualization Tool Server
#'
#' Creates the server logic for the NLDR visualization tool
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @importFrom magrittr %>%
#' @importFrom crosstalk SharedData
#' @importFrom plotly highlight
#' @importFrom FNN get.knn
#' @return A Shiny server function
#' @keywords internal
nldr_viz_server <- function(input, output, session) {
  dataset <- shiny::reactiveVal(NULL)
  vis_results <- shiny::reactiveVal(NULL)
  apply_changes_clicked <- shiny::reactiveVal(FALSE)
  custom_datasets <- shiny::reactiveVal(load_custom_datasets())
  available_datasets <- shiny::reactiveVal(c("None", "four_clusters", "pdfsense", "trees"))

  quollr_model <- shiny::reactiveVal(NULL)
  quollr_results <- shiny::reactiveVal(NULL)
  show_langevitour_flag <- shiny::reactiveVal(FALSE)
  binwidth_optimization_results <- shiny::reactiveVal(list())
  optimal_config <- shiny::reactiveVal(NULL)
  current_dataset_name <- shiny::reactiveVal("Unknown")
  comparison_selected_datasets <- shiny::reactiveVal(character(0))
  comparison_results <- shiny::reactiveVal(NULL)
  nldr_datasets <- shiny::reactiveVal(list())
  shared_vis_data <- shiny::reactiveVal(NULL)
  color_palette <- shiny::reactiveVal(NULL)
  nldr_counter <- shiny::reactiveVal(0)
  active_nldr_id <- shiny::reactiveVal(NULL)

  extract_base_dataset_name <- function(full_name) {
    base_name <- gsub("\\s*-\\s*(t-SNE|UMAP).*$", "", full_name)
    return(trimws(base_name))
  }

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

  shiny::observe({
    choices <- available_datasets()
    shiny::updateSelectInput(session, "example_data", choices = choices)
  })

  shiny::observeEvent(input$example_data, {
    shiny::req(input$example_data != "None")
    custom_data_list <- custom_datasets()
    current_dataset_name(input$example_data)
    data <- custom_data_list[[input$example_data]]
    dataset(data)
    apply_changes_clicked(FALSE)
  })

  shiny::observeEvent(input$file, {
    file <- input$file
    shiny::req(file)
    if (tools::file_ext(file$datapath) != "csv") {
      shiny::showNotification("Only CSV files are supported", type = "error")
      return()
    }
    tryCatch(
      {
        data <- read.csv(file$datapath, stringsAsFactors = TRUE)
        empty_check <- check_empty_cells(data)
        if (empty_check$has_empty) {
          shiny::showModal(shiny::modalDialog(
            title = "Empty Cells Detected",
            shiny::div(
              shiny::p(paste("The dataset contains", empty_check$total_empty, "empty cells.")),
              shiny::p("Affected columns:", paste(empty_check$empty_cols, collapse = ", ")),
              shiny::p("Please clean your data and upload again.")
            ),
            easyClose = TRUE,
            footer = shiny::modalButton("OK")
          ))
          return()
        }
        dataset(data)
        apply_changes_clicked(FALSE)
        dataset_name <- tools::file_path_sans_ext(file$name)
        current_choices <- available_datasets()
        if (dataset_name %in% current_choices) {
          i <- 1
          while (paste0(dataset_name, "_", i) %in% current_choices) {
            i <- i + 1
          }
          dataset_name <- paste0(dataset_name, "_", i)
        }
        current_custom <- custom_datasets()
        current_custom[[dataset_name]] <- data
        custom_datasets(current_custom)
        available_datasets(c(current_choices, dataset_name))
        shiny::showNotification(paste("Dataset", dataset_name, "added to example datasets"), type = "message")
        shiny::updateSelectInput(session, "example_data", selected = dataset_name)
      },
      error = function(e) {
        shiny::showModal(shiny::modalDialog(
          title = "Error Reading File",
          paste("An error occurred while reading the file:", e$message),
          easyClose = TRUE,
          footer = shiny::modalButton("OK")
        ))
      }
    )
    dataset_name <- tools::file_path_sans_ext(file$name)
    current_dataset_name(dataset_name)
  })

  output$column_selection <- shiny::renderUI({
    shiny::req(dataset())
    data <- dataset()
    shiny::tagList(
      shiny::checkboxGroupInput("selected_columns", "Select columns for visualization:",
        choices = names(data)
      ),
      shiny::conditionalPanel(
        condition = "input.apply_changes_clicked",
        shiny::actionButton("reset_columns", "Reset Column Selection", class = "btn-warning")
      )
    )
  })

  shiny::observeEvent(input$reset_columns, {
    shiny::req(dataset())
    orig_data <- dataset()
    if (!is.null(input$file)) {
      file <- input$file
      orig_data <- read.csv(file$datapath, stringsAsFactors = TRUE)
    } else if (input$example_data != "None") {
      custom_data_list <- custom_datasets()
      orig_data <- custom_data_list[[input$example_data]]
    }
    dataset(orig_data)
    shiny::updateCheckboxGroupInput(session, "selected_columns", selected = character(0))
    apply_changes_clicked(FALSE)
  })

  output$color_column_selection <- shiny::renderUI({
    shiny::req(dataset())
    data <- dataset()
    categorical_cols <- names(data)[sapply(data, function(x) is.factor(x) || is.character(x))]
    if (length(categorical_cols) == 0) {
      categorical_cols <- names(data)
    }
    shiny::selectInput("color_column", "Color points by:",
      choices = names(data),
      selected = if (length(categorical_cols) > 0) categorical_cols[1] else names(data)[1]
    )
  })

  shiny::observeEvent(c(input$auto_perplexity, dataset()), {
    shiny::req(dataset())
    data <- dataset()
    if (isTRUE(input$auto_perplexity)) {
      perplexity_value <- min(30, floor(nrow(data) / 3) - 1)
      perplexity_value <- max(5, perplexity_value)
      shiny::updateSliderInput(session, "perplexity", value = perplexity_value)
    }
  })

  shiny::observeEvent(input$perplexity,
    {
      if (isTRUE(input$auto_perplexity)) {
        shiny::updateCheckboxInput(session, "auto_perplexity", value = FALSE)
      }
    },
    ignoreInit = TRUE
  )

  shiny::observeEvent(input$apply_changes, {
    shiny::req(dataset())
    data <- dataset()
    if (!is.null(input$selected_columns)) {
      data <- data[, input$selected_columns, drop = FALSE]
    }
    dataset(data)
    apply_changes_clicked(TRUE)
    shiny::showNotification("Changes applied successfully!", type = "message", duration = 1.5)
  })

  output$data_preview <- DT::renderDT({
    shiny::req(dataset())
    DT::datatable(dataset(), options = list(scrollX = TRUE, pageLength = 10))
  })

  output$data_info <- shiny::renderTable({
    shiny::req(dataset())
    data <- dataset()
    data.frame(
      Metric = c("Number of Rows", "Number of Columns", "Numeric Columns", "Categorical Columns"),
      Value = c(
        nrow(data),
        ncol(data),
        sum(sapply(data, is.numeric)),
        sum(sapply(data, function(x) is.factor(x) || is.character(x)))
      )
    )
  })

  shiny::observeEvent(input$run_visualization, {
    shiny::req(dataset())
    data <- dataset()
    tryCatch(
      {
        empty_check <- check_empty_cells(data)
        if (empty_check$has_empty) {
          shiny::showModal(shiny::modalDialog(title = "Cannot Run", "The dataset contains empty cells."))
          return()
        }

        numeric_cols_idx <- sapply(data, is.numeric)
        if (sum(numeric_cols_idx) < 2) {
          shiny::showNotification("Need at least 2 numeric columns for NLDR", type = "error")
          return()
        }
        numeric_data <- data[, numeric_cols_idx, drop = FALSE]

        variances <- apply(numeric_data, 2, var, na.rm = TRUE)
        zero_var_cols <- names(variances[variances < 1e-10])

        if (length(zero_var_cols) > 0) {
          shiny::showNotification(paste("Removed", length(zero_var_cols), "constant columns."), type = "warning")
          numeric_data <- numeric_data[, !names(numeric_data) %in% zero_var_cols, drop = FALSE]
        }

        if (ncol(numeric_data) < 2) {
          shiny::showModal(shiny::modalDialog(title = "Not Enough Data", "Fewer than 2 varying numeric columns remain."))
          return()
        }

        original_highd_names <- names(numeric_data)
        standardized_names <- paste0("x", seq_along(original_highd_names))
        names(numeric_data) <- standardized_names

        scaled_data <- scale(numeric_data)
        scaled_data[is.na(scaled_data)] <- 0

        color_col <- if (input$auto_color) {
          categorical_cols <- names(data)[sapply(data, function(x) is.factor(x) || is.character(x))]
          if (length(categorical_cols) > 0) categorical_cols[1] else names(data)[1]
        } else {
          input$color_column
        }

        perplexity_value <- input$perplexity
        if (isTRUE(input$auto_perplexity) && input$nldr_method == "t-SNE") {
          perplexity_value <- min(30, floor(nrow(data) / 3) - 1)
          perplexity_value <- max(5, perplexity_value)
        }

        set.seed(input$seed)

        result <- list(method = input$nldr_method, color_col = color_col, color_values = data[[color_col]], seed = input$seed)
        if (input$nldr_method == "t-SNE") {
          shiny::withProgress(message = "Running t-SNE...", {
            max_allowed_perplexity <- (nrow(data) - 1) / 3
            if (perplexity_value >= max_allowed_perplexity) {
              perplexity_value <- floor(max_allowed_perplexity)
              shiny::showNotification(paste("Perplexity too large, using", perplexity_value), type = "warning")
            }
            tsne_result <- Rtsne::Rtsne(scaled_data, dims = 2, perplexity = perplexity_value, max_iter = input$max_iter_tsne, check_duplicates = FALSE, pca = TRUE, verbose = FALSE)
            result$coords <- tsne_result$Y
            result$perplexity <- perplexity_value
            result$max_iter <- input$max_iter_tsne
          })
        } else if (input$nldr_method == "UMAP") {
          shiny::withProgress(message = "Running UMAP...", {
            umap_config <- umap::umap.defaults
            umap_config$n_neighbors <- input$n_neighbors
            umap_config$min_dist <- input$min_dist
            umap_result <- umap::umap(scaled_data, config = umap_config)
            result$coords <- umap_result$layout
            result$n_neighbors <- input$n_neighbors
            result$min_dist <- input$min_dist
          })
        }
        vis_results(result)
        color_as_factor <- as.factor(result$color_values)
        pal <- scales::hue_pal()(length(levels(color_as_factor)))
        names(pal) <- levels(color_as_factor)
        color_palette(pal)
        plot_data_for_shared <- data.frame(x = result$coords[, 1], y = result$coords[, 2], color = result$color_values)
        plot_data_for_shared <- cbind(plot_data_for_shared, as.data.frame(scaled_data))
        shared_vis_data(SharedData$new(plot_data_for_shared, key = ~ row.names(plot_data_for_shared)))
        id <- nldr_counter() + 1
        nldr_counter(id)
        active_nldr_id(as.character(id))
        method_settings <- if (input$nldr_method == "t-SNE") {
          paste0(current_dataset_name(), " - t-SNE (p=", result$perplexity, ")")
        } else {
          paste0(current_dataset_name(), " - UMAP (n=", result$n_neighbors, ")")
        }
        current <- nldr_datasets()
        current[[as.character(id)]] <- list(id = id, name = method_settings, result = result, tour_input_data = plot_data_for_shared, timestamp = Sys.time())
        nldr_datasets(current)

        quollr_results(NULL)
        optimal_config(NULL)
        show_langevitour_flag(FALSE)
        shiny::showNotification("Previous analysis results cleared.", type = "message")
      },
      error = function(e) {
        shiny::showModal(shiny::modalDialog(
          title = "Visualization Error",
          paste("An error occurred:", e$message),
          easyClose = TRUE
        ))
      }
    )
  })


  nldr_plotly_object <- shiny::reactive({
    shiny::req(shared_vis_data(), vis_results())
    sd_obj <- shared_vis_data()
    result <- vis_results()

    p <- ggplot2::ggplot(sd_obj, ggplot2::aes(x = x, y = y, color = color, text = color)) +
      ggplot2::geom_point(size = 2, alpha = 0.7) +
      ggplot2::labs(
        x = paste(result$method, "Dimension 1"),
        y = paste(result$method, "Dimension 2"),
        color = result$color_col
      ) +
      ggplot2::coord_fixed(ratio = 1) +
      ggplot2::theme_minimal()

    plot <- plotly::ggplotly(p, tooltip = "text")

    if (isTRUE(input$enable_brushing)) {
      plot <- plotly::highlight(plot, on = "plotly_selected", off = "plotly_doubleclick", opacityDim = 0.2)
    }

    plotly::layout(plot, autosize = TRUE)
  })

  output$nldr_plot <- plotly::renderPlotly({
    nldr_plotly_object()
  })
  output$nldr_plot_tour_tab <- plotly::renderPlotly({
    nldr_plotly_object()
  })

  output$dynamic_tour_output_ui <- shiny::renderUI({
    shiny::req(shared_vis_data(), input$tour_display_type)
    detourr::displayScatter2dOutput("tour_plot_2d", height = "580px")
  })

  tour_edges <- shiny::reactive({
    shiny::req(input$show_edges, shared_vis_data())
    data <- shared_vis_data()$data()
    projection_cols <- setdiff(names(data), c("x", "y", "color"))

    if (nrow(data) < 2 || length(projection_cols) < 1) {
      return(NULL)
    }

    knn <- FNN::get.knn(data[, projection_cols, drop = FALSE], k = 5)
    from <- rep(1:nrow(data), 5)
    to <- as.vector(t(knn$nn.index))
    cbind(from, to)
  })

  tour_object <- shiny::reactive({
    shiny::req(shared_vis_data(), input$tour_display_type, !is.null(input$tour_axes), !is.null(input$show_edges), color_palette())
    sd_obj <- shared_vis_data()
    pal <- color_palette()
    projection_cols <- setdiff(names(sd_obj$data()), c("x", "y", "color"))

    if (length(projection_cols) < 2) {
      shiny::showNotification("Not enough high-dimensional columns for the tour.", type = "warning")
      return(NULL)
    }

    detour_obj <- detourr::detour(
      sd_obj,
      detourr::tour_aes(projection = projection_cols, colour = color)
    ) |>
      detourr::tour_path(tourr::grand_tour(2L), fps = 30)

    current_edges <- if (isTRUE(input$show_edges)) tour_edges() else NULL

    switch(input$tour_display_type,
      "Scatter" = {
        shiny::req(input$tour_alpha)
        detour_obj |> detourr::show_scatter(alpha = input$tour_alpha, axes = input$tour_axes, palette = pal, size = 1, edges = current_edges)
      },
      "Sage" = {
        shiny::req(input$tour_gamma)
        detour_obj |> detourr::show_sage(gamma = input$tour_gamma, axes = input$tour_axes, palette = pal, size = 1, edges = current_edges)
      },
      "Slice" = {
        shiny::req(input$tour_slice_volume)
        detour_obj |> detourr::show_slice(slice_relative_volume = input$tour_slice_volume, axes = input$tour_axes, palette = pal, size = 1, edges = current_edges)
      }
    )
  })

  output$tour_plot_2d <- detourr::shinyRenderDisplayScatter2d({
    tour_object()
  })

  output$vis_info <- shiny::renderPrint({
    shiny::req(vis_results())
    result <- vis_results()
    cat(result$method, "Visualization\n-------------------\n")
    if (result$method == "t-SNE") {
      cat("Perplexity:", result$perplexity, "\n")
      cat("Max Iterations:", result$max_iter, "\n")
    } else {
      cat("Number of Neighbors:", result$n_neighbors, "\n")
      cat("Minimum Distance:", result$min_dist, "\n")
    }
    cat("Random Seed:", result$seed, "\n")
    cat("Color Column:", result$color_col, "\n")
  })

  output$stored_nldr_ui <- shiny::renderUI({
    datasets <- nldr_datasets()
    if (length(datasets) == 0) {
      return(shiny::div(class = "text-center text-muted p-3", shiny::p("No stored NLDR results yet.")))
    }

    lapply(datasets, function(ds) {
      shiny::div(
        class = "stored-dataset-item d-flex align-items-center justify-content-between p-2 mb-2 border rounded",
        shiny::div(class = "form-check me-3", shiny::checkboxInput(inputId = paste0("nldr_", ds$id), label = NULL)),
        shiny::div(class = "dataset-name flex-grow-1", title = ds$name, shiny::tags$strong(ds$name)),
        shiny::downloadButton(outputId = paste0("download_nldr_", ds$id), label = NULL, icon = shiny::icon("download"), class = "btn btn-sm")
      )
    })
  })

  shiny::observe({
    datasets <- nldr_datasets()
    lapply(names(datasets), function(ds_id) {
      output[[paste0("download_nldr_", ds_id)]] <- shiny::downloadHandler(
        filename = function() {
          paste0("nldr_", gsub("\\s+", "_", datasets[[ds_id]]$name), ".rds")
        },
        content = function(file) {
          saveRDS(datasets[[ds_id]], file)
        }
      )
    })
  })

  shiny::observe({
    datasets <- nldr_datasets()
    lapply(names(datasets), function(ds_id) {
      shiny::observeEvent(input[[paste0("nldr_", ds_id)]], {
        shiny::req(input[[paste0("nldr_", ds_id)]])
        ds <- datasets[[ds_id]]
        active_nldr_id(ds_id)
        vis_results(ds$result)
        shared_vis_data(SharedData$new(ds$tour_input_data, key = ~ row.names(ds$tour_input_data)))
        shiny::showNotification(paste("Loaded:", ds$name), type = "message")
        shiny::updateCheckboxInput(session, paste0("nldr_", ds_id), value = FALSE)
      })
    })
  })

  shiny::observeEvent(c(input$auto_bin_range, shared_vis_data()), {
    shiny::req(shared_vis_data(), isTRUE(input$auto_bin_range))
    n_points <- nrow(shared_vis_data()$data())
    min_suggested <- max(5, floor(sqrt(n_points / 50)))
    max_suggested <- min(25, floor(sqrt(n_points / 5)))
    if (max_suggested <= min_suggested) max_suggested <- min_suggested + 5
    shiny::updateNumericInput(session, "min_bins", value = min_suggested)
    shiny::updateNumericInput(session, "max_bins", value = max_suggested)
  })

  shiny::observeEvent(input$run_quollr_analysis, {
    shiny::req(shared_vis_data(), optimal_config())

    tryCatch(
      {
        shiny::withProgress(message = "Building Quollr Model...", {
          vis_data <- shared_vis_data()$data()
          optimal <- optimal_config()
          benchmark_val <- if (isTRUE(input$quollr_remove_low_density)) {
            input$quollr_density_threshold
          } else {
            0
          }

          highd_cols <- setdiff(names(vis_data)[sapply(vis_data, is.numeric)], c("x", "y", "color"))
          highd_data <- vis_data[, highd_cols, drop = FALSE]
          highd_data$ID <- seq_len(nrow(highd_data))

          nldr_data_raw <- data.frame(emb1 = vis_data$x, emb2 = vis_data$y, ID = seq_len(nrow(vis_data)))

          shiny::incProgress(0.2, detail = "Scaling and binning...")
          nldr_obj <- quollr::gen_scaled_data(nldr_data = nldr_data_raw)
          hb_obj <- quollr::hex_binning(nldr_obj = nldr_obj, b1 = optimal$b1)

          all_centroids_df <- hb_obj$centroids
          counts_df <- hb_obj$std_cts

          df_bin_centroids <- quollr::extract_hexbin_centroids(
            centroids_data = all_centroids_df,
            counts_data = counts_df
          )

          model_2d <- df_bin_centroids |>
            dplyr::filter(n_h > benchmark_val)

          shiny::req(nrow(model_2d) >= 3,
            cancelOutput = TRUE,
            message = "Not enough dense hexagons found to build a model. Please lower the 'Point Count Threshold' or uncheck the box."
          )

          shiny::incProgress(0.3, detail = "Triangulating...")
          tr_object <- quollr::tri_bin_centroids(centroids_data = model_2d)

          trimesh_data <- quollr::gen_edges(tri_object = tr_object, a1 = hb_obj$a1)

          shiny::incProgress(0.3, detail = "Lifting to high-D...")
          nldr_df_with_hex_id <- hb_obj$data_hb_id
          model_highd <- quollr::avg_highd_data(highd_data = highd_data, scaled_nldr_hexid = nldr_df_with_hex_id)
          model_highd <- model_highd |> dplyr::filter(h %in% model_2d$h)

          quollr_results(
            list(
              highd_data = highd_data,
              model_2d = model_2d,
              model_highd = model_highd,
              trimesh_data = trimesh_data,
              nldr_obj = nldr_obj,
              hb_obj = hb_obj
            )
          )

          show_langevitour_flag(FALSE)
          shiny::showNotification("Quollr analysis completed successfully!", type = "message")
        })
      },
      error = function(e) {
        shiny::showModal(shiny::modalDialog(
          title = "Analysis Error",
          if (grepl("Not enough dense hexagons", e$message, fixed = TRUE)) {
            e$message
          } else {
            paste("An unexpected error occurred:", e$message)
          }
        ))
      }
    )
  })

  shiny::observeEvent(input$run_binwidth_optimization, {
    shiny::req(shared_vis_data(), vis_results())
    tryCatch(
      {
        shiny::withProgress(message = "Optimizing binwidth...", {
          vis_data <- shared_vis_data()$data()
          highd_cols <- setdiff(names(vis_data)[sapply(vis_data, is.numeric)], c("x", "y", "color"))
          highd_data <- vis_data[, highd_cols, drop = FALSE]
          highd_data$ID <- seq_len(nrow(highd_data))
          nldr_data <- data.frame(emb1 = vis_data$x, emb2 = vis_data$y, ID = seq_len(nrow(vis_data)))

          error_df_all <- quollr::gen_diffbin1_errors(highd_data = highd_data, nldr_data = nldr_data)

          if (is.null(error_df_all) || nrow(error_df_all) == 0) stop("Optimization failed.")

          processed_results <- error_df_all %>%
            dplyr::filter(b1 >= 5) %>%
            dplyr::group_by(a1) %>%
            dplyr::slice_min(RMSE, n = 1, with_ties = FALSE) %>%
            dplyr::ungroup() %>%
            dplyr::arrange(a1)

          optimal_row <- processed_results %>% dplyr::slice_min(RMSE, n = 1)

          shiny::req(active_nldr_id())
          current_vis_id <- active_nldr_id()

          all_stored_results <- binwidth_optimization_results()
          all_stored_results[[current_vis_id]] <- processed_results
          binwidth_optimization_results(all_stored_results)
          optimal_config(optimal_row)

          shiny::showNotification("Optimization complete!", type = "message")
        })
      },
      error = function(e) {
        shiny::showModal(shiny::modalDialog(title = "Optimization Error", e$message))
      }
    )
  })

  output$binwidth_mse_plot <- plotly::renderPlotly({
    all_results <- binwidth_optimization_results()
    shiny::req(active_nldr_id())
    current_key <- active_nldr_id()
    results_for_plot <- all_results[[current_key]]
    shiny::req(results_for_plot, optimal_config())
    optimal <- optimal_config()
    p <- results_for_plot %>%
      dplyr::arrange(a1) %>%
      ggplot2::ggplot(ggplot2::aes(x = a1, y = RMSE, text = paste("b1:", b1), group = 1)) +
      ggplot2::geom_line(linewidth = 0.7, color = "steelblue") +
      ggplot2::geom_point(size = 1.5, color = "steelblue") +
      ggplot2::labs(title = "RMSE vs. Binwidth (a1)", x = "Binwidth (a1)", y = "RMSE") +
      ggplot2::theme_minimal()
    plotly::ggplotly(p, tooltip = c("x", "y", "text"))
  })
  output$binwidth_results_table <- DT::renderDT({
    all_results <- binwidth_optimization_results()
    shiny::req(shared_vis_data())
    current_key <- names(nldr_datasets())[length(nldr_datasets())]
    results_for_plot <- all_results[[current_key]]
    shiny::req(results_for_plot)
    DT::datatable(results_for_plot, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  output$quollr_fit_plot <- plotly::renderPlotly({
    shiny::req(quollr_results())
    model_results <- quollr_results()
    pred_data <- quollr::predict_emb(
      highd_data = model_results$highd_data,
      model_highd = model_results$model_highd,
      model_2d = model_results$model_2d
    )

    plot_data <- model_results$nldr_obj$scaled_nldr %>%
      dplyr::left_join(pred_data, by = "ID") %>%
      dplyr::mutate(Residual = sqrt((emb1 - pred_emb_1)^2 + (emb2 - pred_emb_2)^2))

    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = emb1, y = emb2, color = Residual)) +
      ggplot2::geom_point(alpha = 0.7) +
      ggplot2::scale_color_viridis_c() +
      ggplot2::coord_fixed() +
      ggplot2::labs(title = "Model Fit Quality (Prediction Error)", x = "Embedding Dim 1", y = "Embedding Dim 2") +
      ggplot2::theme_minimal()

    plotly::ggplotly(p)
  })

  shiny::observeEvent(input$show_langevitour, {
    shiny::req(quollr_results())
    show_langevitour_flag(TRUE)
  })
  output$show_langevitour_ui <- shiny::reactive({
    show_langevitour_flag()
  })
  shiny::outputOptions(output, "show_langevitour_ui", suspendWhenHidden = FALSE)

  output$langevitour_output <- shiny::renderUI({
    shiny::req(show_langevitour_flag(), quollr_results())
    results <- quollr_results()
    tour_data <- quollr::comb_data_model(
      highd_data  = results$highd_data,
      model_highd = results$model_highd,
      model_2d    = results$model_2d
    )
    quollr::show_langevitour(
      point_data = tour_data,
      edge_data  = results$trimesh_data
    )
  })

  output$optimal_binwidth_summary <- shiny::renderPrint({
    shiny::req(optimal_config())
    optimal <- optimal_config()
    cat("Optimal Binwidth (a1):", round(optimal$a1, 3), "\n")
    cat("Corresponding Bins (b1):", optimal$b1, "\n")
    cat("Resulting RMSE:", round(optimal$RMSE, 5), "\n")
  })

  output$quollr_model_summary <- shiny::renderPrint({
    shiny::req(quollr_results())
    glance_df <- quollr::glance(
      highd_data = quollr_results()$highd_data,
      model_highd = quollr_results()$model_highd,
      model_2d = quollr_results()$model_2d
    )
    cat("Model Fit Summary\n-----------------\n")
    cat("Total Error:", round(glance_df$Error, 2), "\n")
    cat("RMSE:", round(glance_df$RMSE, 4), "\n")
    cat("Model Bins:", nrow(quollr_results()$model_2d), "\n")
  })

  output$comparison_dataset_selection <- shiny::renderUI({
    datasets <- nldr_datasets()
    if (length(datasets) < 2) {
      return(shiny::p("Run at least two NLDR visualizations to compare them."))
    }

    dataset_names <- sapply(datasets, function(ds) ds$name)
    base_names <- sapply(dataset_names, extract_base_dataset_name)
    grouped_choices <- split(names(datasets), base_names)

    lapply(names(grouped_choices), function(base_name) {
      shiny::div(
        class = "dataset-group mb-3",
        shiny::h6(base_name, class = "text-primary"),
        lapply(grouped_choices[[base_name]], function(ds_id) {
          ds <- datasets[[ds_id]]
          shiny::checkboxInput(
            inputId = paste0("comp_select_", ds_id),
            label = ds$name,
            value = ds_id %in% comparison_selected_datasets()
          )
        })
      )
    })
  })

  shiny::observe({
    datasets <- nldr_datasets()
    if (length(datasets) == 0) {
      comparison_selected_datasets(character(0))
      return()
    }
    selected_ids <- names(datasets)[sapply(names(datasets), function(ds_id) {
      isTRUE(input[[paste0("comp_select_", ds_id)]])
    })]
    comparison_selected_datasets(selected_ids)
  })

  shiny::observeEvent(input$clear_comparison_selection, {
    datasets <- nldr_datasets()
    lapply(names(datasets), function(ds_id) {
      shiny::updateCheckboxInput(session, paste0("comp_select_", ds_id), value = FALSE)
    })
  })

  shiny::observeEvent(input$run_comparison_analysis, {
    selected_ids <- comparison_selected_datasets()
    if (length(selected_ids) < 2) {
      return(shiny::showModal(shiny::modalDialog(title = "Insufficient Selection", "Please select at least 2 datasets.")))
    }
    tryCatch(
      {
        shiny::withProgress(message = "Generating comparison...", {
          stored_results <- binwidth_optimization_results()
          datasets_info <- nldr_datasets()
          results_to_compare <- list()

          for (ds_id in selected_ids) {
            if (ds_id %in% names(stored_results)) {
              df <- stored_results[[ds_id]]
              df$dataset_name <- datasets_info[[ds_id]]$name
              df$method <- datasets_info[[ds_id]]$result$method
              results_to_compare[[ds_id]] <- df
            } else {
              shiny::showNotification(paste("Run 'Optimize Binwidth' for", datasets_info[[ds_id]]$name, "first."), type = "warning")
            }
          }

          if (length(results_to_compare) > 0) {
            comparison_results(dplyr::bind_rows(results_to_compare))
            shiny::showNotification("Comparison plot generated!", type = "message")
          }
        })
      },
      error = function(e) {
        shiny::showModal(shiny::modalDialog(title = "Comparison Error", e$message))
      }
    )
  })

  output$comparison_mse_plot <- plotly::renderPlotly({
    shiny::req(comparison_results())
    results <- comparison_results()

    p <- ggplot2::ggplot(results, ggplot2::aes(x = a1, y = RMSE, color = method, linetype = dataset_name)) +
      ggplot2::geom_line() +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Method Comparison: RMSE vs. Binwidth", x = "Binwidth (a1)", y = "RMSE", color = "Method", linetype = "Dataset") +
      ggplot2::theme_minimal()

    plotly::ggplotly(p)
  })

  output$best_configuration_summary <- shiny::renderPrint({
    shiny::req(comparison_results())
    results <- comparison_results()
    if (nrow(results) == 0) {
      return("No comparison data available.")
    }

    overall_best <- results %>% dplyr::slice_min(RMSE, n = 1, with_ties = FALSE)

    cat("🏆 Best Configuration Found\n--------------------------\n")
    cat("Name:", overall_best$dataset_name, "\n")
    cat("Method:", overall_best$method, "\n")
    cat("Optimal Binwidth (a1):", round(overall_best$a1, 3), "\n")
    cat("Corresponding Bins (b1):", overall_best$b1, "\n")
    cat("Resulting RMSE:", round(overall_best$RMSE, 5), "\n")
  })
}
