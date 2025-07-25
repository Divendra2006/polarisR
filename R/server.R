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
  binwidth_optimization_results <- shiny::reactiveVal(NULL)
  optimal_config <- shiny::reactiveVal(NULL)

  nldr_datasets <- shiny::reactiveVal(list())
  shared_vis_data <- shiny::reactiveVal(NULL)
  color_palette <- shiny::reactiveVal(NULL)
  nldr_counter <- shiny::reactiveVal(0)

  visualization_config <- shiny::reactive({
    list(
      show_hexagons = if(is.null(input$quollr_show_hexagons)) FALSE else input$quollr_show_hexagons,
      color_by_density = if(is.null(input$quollr_color_by_density)) TRUE else input$quollr_color_by_density,
      remove_low_density = if(is.null(input$quollr_remove_low_density)) FALSE else input$quollr_remove_low_density,
      density_threshold = if(is.null(input$quollr_density_threshold)) 0.1 else input$quollr_density_threshold
    )
  })

  check_empty_cells <- function(data) {
    if (is.null(data) || nrow(data) == 0) return(list(has_empty = FALSE))
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

  validate_quollr_data <- function(vis_data) {
    issues <- character(0)

    if (nrow(vis_data) < 10) {
      issues <- c(issues, "Dataset too small (need at least 10 points)")
    }

    required_cols <- c("x", "y")
    missing_cols <- setdiff(required_cols, names(vis_data))
    if (length(missing_cols) > 0) {
      issues <- c(issues, paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
    }

    numeric_cols <- sapply(vis_data, is.numeric)
    excluded_cols <- c("x", "y", "color")
    available_cols <- names(vis_data)[numeric_cols]
    highd_cols <- setdiff(available_cols, excluded_cols)

    if (length(highd_cols) < 2) {
      issues <- c(issues, "Need at least 2 high-dimensional numeric columns")
    }

    if (any(is.na(vis_data$x)) || any(is.na(vis_data$y))) {
      issues <- c(issues, "Missing values found in x or y coordinates")
    }

    return(issues)
  }

  shiny::observe({
    choices <- available_datasets()
    shiny::updateSelectInput(session, "example_data", choices = choices)
  })

  shiny::observeEvent(input$example_data, {
    shiny::req(input$example_data != "None")
    custom_data_list <- custom_datasets()
    data <- custom_data_list[[input$example_data]]
    dataset(data)
    apply_changes_clicked(FALSE)
  })

  shiny::observeEvent(input$file, {
    file <- input$file
    shiny::req(file)
    if(tools::file_ext(file$datapath) != "csv") {
      shiny::showNotification("Only CSV files are supported", type = "error")
      return()
    }
    tryCatch({
      data <- read.csv(file$datapath, stringsAsFactors = TRUE)
      empty_check <- check_empty_cells(data)
      if (empty_check$has_empty) {
        shiny::showModal(shiny::modalDialog(
          title = "Empty Cells Detected",
          shiny::div(
            shiny::p(paste("The dataset contains", empty_check$total_empty, "empty cells.")),
            shiny::p("Affected columns:", paste(empty_check$empty_cols, collapse = ", ")),
            shiny::p("Please clean your data and upload again. Visualization cannot proceed with empty cells.")
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
        while(paste0(dataset_name, "_", i) %in% current_choices) {
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
    }, error = function(e) {
      shiny::showModal(shiny::modalDialog(
        title = "Error Reading File",
        paste("An error occurred while reading the file:", e$message),
        easyClose = TRUE,
        footer = shiny::modalButton("OK")
      ))
    })
  })

  output$column_selection <- shiny::renderUI({
    shiny::req(dataset())
    data <- dataset()
    shiny::tagList(
      shiny::checkboxGroupInput("selected_columns", "Select columns for visualization:",
                                choices = names(data),
                                shiny::conditionalPanel(
                                  condition = "input.apply_changes_clicked",
                                  shiny::actionButton("reset_columns", "Reset Column Selection", class = "btn-warning")
                                )
      ))
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
                       selected = if(length(categorical_cols) > 0) categorical_cols[1] else names(data)[1])
  })

  shiny::observeEvent(c(input$auto_perplexity, dataset()), {
    shiny::req(dataset())
    data <- dataset()
    if (input$auto_perplexity) {
      perplexity_value <- min(30, floor(nrow(data) / 3) - 1)
      perplexity_value <- max(5, perplexity_value)
      shiny::updateSliderInput(session, "perplexity", value = perplexity_value)
    }
  });

  shiny::observeEvent(input$perplexity, {
    if (input$auto_perplexity) {
      shiny::updateCheckboxInput(session, "auto_perplexity", value = FALSE)
    }
  }, ignoreInit = TRUE)

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
    tryCatch({
      empty_check <- check_empty_cells(data)
      if (empty_check$has_empty) {
        shiny::showModal(shiny::modalDialog(
          title = "Cannot Run Visualization",
          shiny::div(
            shiny::p("The dataset contains empty cells."),
            shiny::p("Please clean your data before running visualization."),
            shiny::p("Affected columns:", paste(empty_check$empty_cols, collapse = ", "))
          ),
          easyClose = TRUE,
          footer = shiny::modalButton("OK")
        ))
        return()
      }
      numeric_cols <- sapply(data, is.numeric)
      if (sum(numeric_cols) < 2) {
        shiny::showNotification("Need at least 2 numeric columns for dimensionality reduction", type = "error")
        return()
      }
      numeric_data <- data[, numeric_cols, drop = FALSE]
      scaled_data <- scale(numeric_data)
      scaled_data[is.na(scaled_data)] <- 0
      color_col <- if (input$auto_color) {
        categorical_cols <- names(data)[sapply(data, function(x) is.factor(x) || is.character(x))]
        if (length(categorical_cols) > 0) {
          categorical_cols[1]
        } else {
          names(data)[1]
        }
      } else {
        input$color_column
      }
      perplexity_value <- input$perplexity
      if (input$auto_perplexity && input$nldr_method == "t-SNE") {
        perplexity_value <- min(30, floor(nrow(data) / 3) - 1)
        perplexity_value <- max(5, perplexity_value)
      }
      set.seed(input$seed)
      result <- list(
        method = input$nldr_method,
        color_col = color_col,
        color_values = data[[color_col]],
        seed = input$seed
      )
      if (input$nldr_method == "t-SNE") {
        shiny::withProgress(message = 'Running t-SNE...', {
          max_allowed_perplexity <- nrow(data) - 1
          if (perplexity_value >= max_allowed_perplexity) {
            perplexity_value <- max_allowed_perplexity / 3
            shiny::showNotification(
              paste("Perplexity was too large for this dataset. Using", perplexity_value, "instead."),
              type = "warning"
            )
          }
          tsne_result <- Rtsne::Rtsne(scaled_data, dims = 2, perplexity = perplexity_value,
                                      max_iter = input$max_iter_tsne, check_duplicates = FALSE,
                                      pca = TRUE, pca_center = TRUE, pca_scale = FALSE,
                                      theta = 0.5, verbose = FALSE)
          result$coords <- tsne_result$Y
          result$perplexity <- perplexity_value
          result$max_iter <- input$max_iter_tsne
        })
      } else if (input$nldr_method == "UMAP") {
        shiny::withProgress(message = 'Running UMAP...', {
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
      color_lvls <- levels(color_as_factor)

      pal <- scales::hue_pal()(length(color_lvls))
      names(pal) <- color_lvls
      color_palette(pal)
      plot_data_for_shared <- data.frame(
        x = result$coords[, 1],
        y = result$coords[, 2],
        color = result$color_values
      )
      plot_data_for_shared <- cbind(plot_data_for_shared, as.data.frame(scaled_data))

      shared_vis_data(SharedData$new(plot_data_for_shared, key = ~row.names(plot_data_for_shared)))

      id <- nldr_counter() + 1
      nldr_counter(id)
      method_settings <- if(input$nldr_method == "t-SNE") {
        paste0("t-SNE (p=", result$perplexity, ", iter=", result$max_iter, ")")
      } else {
        paste0("UMAP (n=", result$n_neighbors, ", dist=", result$min_dist, ")")
      }
      current <- nldr_datasets()
      current[[as.character(id)]] <- list(
        id = id,
        name = method_settings,
        result = result,
        tour_input_data = plot_data_for_shared,
        timestamp = Sys.time()
      )
      nldr_datasets(current)

    }, error = function(e) {
      shiny::showModal(shiny::modalDialog(
        title = "Visualization Error",
        paste("An error occurred during visualization:", e$message),
        easyClose = TRUE,
        footer = shiny::modalButton("OK")
      ))
    })
  })

  nldr_plotly_object <- shiny::reactive({
    shiny::req(shared_vis_data())
    sd_obj <- shared_vis_data()
    result <- vis_results()
    pal <- color_palette()

    p <- ggplot2::ggplot(sd_obj, ggplot2::aes(x = x, y = y, color = color, text = color)) +
      ggplot2::geom_point(size = 2, alpha = 0.7) +
      ggplot2::labs(
        x = paste(result$method, "Dimension 1"),
        y = paste(result$method, "Dimension 2"),
        color = result$color_col) +
      ggplot2::coord_fixed(ratio = 1) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "right")

    x_range <- range(sd_obj$data()$x, na.rm = TRUE)
    y_range <- range(sd_obj$data()$y, na.rm = TRUE)

    overall_range <- range(c(x_range, y_range))
    axis_padding <- diff(overall_range) * 0.05

    axis_min <- overall_range[1] - axis_padding
    axis_max <- overall_range[2] + axis_padding

    if (isTRUE(input$enable_brushing)) {
      plotly::layout(
        plotly::highlight(plotly::ggplotly(p, tooltip = "text"),
                          on = "plotly_selected",
                          off = "plotly_doubleclick",
                          opacityDim = 0.2),
        autosize = TRUE,
        legend = list(title = list(text = result$color_col)),
        xaxis = list(
          range = c(axis_min, axis_max),
          constrain = "domain",
          scaleanchor = "y",
          scaleratio = 1
        ),
        yaxis = list(
          range = c(axis_min, axis_max),
          constrain = "domain"
        )
      )
    } else {
      plotly::layout(
        plotly::ggplotly(p, tooltip = "text"),
        autosize = TRUE,
        legend = list(title = list(text = result$color_col)),
        xaxis = list(
          range = c(axis_min, axis_max),
          constrain = "domain",
          scaleanchor = "y",
          scaleratio = 1
        ),
        yaxis = list(
          range = c(axis_min, axis_max),
          constrain = "domain"
        )
      )
    }
  })

  output$nldr_plot <- plotly::renderPlotly({
    my_plot <- nldr_plotly_object()
    my_plot %>%
      plotly::layout(
        margin = list(
          l = 50,
          r = 50,
          b = 100,
          t = 50
        ),
        xaxis = list(automargin = TRUE, title = list(standoff = 20)),
        yaxis = list(automargin = TRUE, title = list(standoff = 20))
      )
  })

  output$nldr_plot_tour_tab <- plotly::renderPlotly({
    my_plot <- nldr_plotly_object()
    my_plot %>%
      plotly::layout(
        margin = list(
          l = 50,
          r = 50,
          b = 50,
          t = 20
        ),
        xaxis = list(
          automargin = TRUE,
          title = list(standoff = 10)
        ),
        yaxis = list(
          automargin = TRUE,
          title = list(standoff = 10)
        ),
        plot_bgcolor = 'rgba(0,0,0,0)',
        paper_bgcolor = 'rgba(0,0,0,0)'
      )
  })

  output$dynamic_tour_output_ui <- shiny::renderUI({
    shiny::req(
      shared_vis_data(),
      input$tour_display_type
    )
    input$show_edges
    detourr::displayScatter2dOutput("tour_plot_2d", height = "580px")
  })

  tour_edges <- shiny::reactive({
    shiny::req(input$show_edges, shared_vis_data())

    data <- shared_vis_data()$data()
    all_cols <- names(data)
    projection_cols <- all_cols[!all_cols %in% c("x", "y", "color")]

    # Ensure there is data to process
    if (nrow(data) < 2 || length(projection_cols) < 1) {
      return(NULL)
    }

    # Calculate 5-nearest neighbors using the high-dimensional data
    knn <- FNN::get.knn(data[, projection_cols, drop = FALSE], k = 5)

    # Format the k-NN results into an edge list (from-to matrix)
    from <- rep(1:nrow(data), 5)
    to <- as.vector(t(knn$nn.index))
    edges <- cbind(from, to)

    return(edges)
  })

  tour_object <- shiny::reactive({
    shiny::req(
      shared_vis_data(),
      input$tour_display_type,
      !is.null(input$tour_axes),
      !is.null(input$show_edges),
      color_palette()
    )

    sd_obj <- shared_vis_data()
    pal <- color_palette()
    all_cols <- names(sd_obj$data())
    projection_cols <- all_cols[!all_cols %in% c("x", "y", "color")]

    current_edges <- if (isTRUE(input$show_edges)) {
      tour_edges()
    } else {
      NULL
    }

    if (length(projection_cols) < 2) {
      shiny::showNotification(
        "Not enough high-dimensional columns for the tour. Need at least 2.",
        type = "warning", duration = 3
      )
      return(NULL)
    }
    data_for_tour <- if (isTRUE(input$enable_brushing)) {
      sd_obj
    } else {
      sd_obj$data()
    }
    detour_obj <- detourr::detour(
      data_for_tour,
      detourr::tour_aes(projection = projection_cols,
                        colour = color)
    ) |>
      detourr::tour_path(tourr::grand_tour(2L), fps = 30)
    switch(
      input$tour_display_type,
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

    if (result$method == "t-SNE") {
      cat("t-SNE Visualization\n")
      cat("-------------------\n")
      cat("Perplexity:", result$perplexity, "\n")
      cat("Max Iterations:", result$max_iter, "\n")
      cat("Random Seed:", result$seed, "\n")
      cat("Color Column:", result$color_col, "\n")
    } else {
      cat("UMAP Visualization\n")
      cat("-----------------\n")
      cat("Number of Neighbors:", result$n_neighbors, "\n")
      cat("Minimum Distance:", result$min_dist, "\n")
      cat("Random Seed:", result$seed, "\n")
      cat("Color Column:", result$color_col, "\n")
    }
  })

  output$stored_nldr_ui <- shiny::renderUI({
    datasets <- nldr_datasets()
    if (length(datasets) == 0) {
      return(
        shiny::div(
          class = "text-center text-muted p-3",
          shiny::icon("database", class = "fa-2x mb-2"),
          shiny::p("No stored NLDR datasets yet.", class = "mb-0")
        )
      )
    }

    shiny::div(
      class = "stored-datasets-container",
      style = "max-height: 350px; overflow-y: auto;",
      lapply(datasets, function(ds) {
        shiny::div(
          class = "stored-dataset-item d-flex align-items-center justify-content-between p-2 mb-2 border rounded",
          shiny::div(
            class = "d-flex align-items-center",
            shiny::div(
              class = "form-check me-3",
              shiny::checkboxInput(
                inputId = paste0("nldr_", ds$id),
                label = NULL,
                value = FALSE
              )
            ),
            shiny::div(
              class = "dataset-name flex-grow-1",
              title = ds$name,
              shiny::tags$strong(ds$name)
            )
          ),
          shiny::div(
            class = "download-button",
            shiny::downloadButton(
              outputId = paste0("download_nldr_", ds$id),
              label = NULL,
              icon = shiny::icon("download"),
              class = "btn btn-sm btn-outline-primary",
              title = "Download dataset"
            )
          )
        )
      })
    )
  })

  shiny::observe({
    datasets <- nldr_datasets()
    lapply(datasets, function(ds) {
      output[[paste0("download_nldr_", ds$id)]] <- shiny::downloadHandler(
        filename = function() {
          paste0("nldr_", gsub("\\s+", "_", ds$name), ".zip")
        },
        content = function(file) {
          temp_dir <- tempdir()
          ds_dir <- file.path(temp_dir, ds$name)
          dir.create(ds_dir)
          coords_file <- file.path(ds_dir, "coordinates.csv")
          coords_data <- data.frame(
            x = ds$result$coords[,1],
            y = ds$result$coords[,2],
            color = ds$result$color_values
          )
          utils::write.csv(coords_data, coords_file, row.names = FALSE)
          settings_file <- file.path(ds_dir, "settings.json")
          settings <- list(
            method = ds$result$method,
            seed = ds$result$seed,
            color_column = ds$result$color_col,
            timestamp = ds$timestamp
          )
          if (ds$result$method == "t-SNE") {
            settings$perplexity <- ds$result$perplexity
            settings$max_iter <- ds$result$max_iter
          } else {
            settings$n_neighbors <- ds$result$n_neighbors
            settings$min_dist <- ds$result$min_dist
          }
          writeLines(jsonlite::toJSON(settings, pretty = TRUE), settings_file)
          zip_file <- file.path(temp_dir, paste0("nldr_", ds$name, ".zip"))
          zip::zip(zip_file, files = list.files(ds_dir, full.names = TRUE), recurse = FALSE)
          file.copy(zip_file, file)
        }
      )
    })
  })

  shiny::observe({
    datasets <- nldr_datasets()
    if (length(datasets) == 0) return()
    lapply(datasets, function(ds) {
      input_id <- paste0("nldr_", ds$id)
      if (!is.null(input[[input_id]]) && input[[input_id]]) {
        vis_results(ds$result)
        loaded_data <- ds$tour_input_data
        color_as_factor <- as.factor(loaded_data$color)
        color_lvls <- levels(color_as_factor)
        pal <- scales::hue_pal()(length(color_lvls))
        names(pal) <- color_lvls
        color_palette(pal)
        loaded_data$color <- color_as_factor
        shared_vis_data(SharedData$new(loaded_data, key = ~row.names(loaded_data)))

        shiny::updateCheckboxInput(session, input_id, value = FALSE)
        shiny::showNotification(paste("Loaded NLDR dataset:", ds$name),
                                type = "message", duration = 2)
      }
    })
  })

  shiny::observeEvent(c(input$auto_bin_range, shared_vis_data()), {
    shiny::req(shared_vis_data())

    if (input$auto_bin_range) {
      data <- shared_vis_data()$data()
      n_points <- nrow(data)

      min_suggested <- max(3, floor(sqrt(n_points/100)))
      max_suggested <- min(25, floor(sqrt(n_points/5)))

      if (max_suggested <= min_suggested) {
        max_suggested <- min_suggested + 5
      }

      shiny::updateNumericInput(session, "min_bins", value = min_suggested)
      shiny::updateNumericInput(session, "max_bins", value = max_suggested)

      shiny::showNotification(
        paste("Auto-calculated range:", min_suggested, "to", max_suggested),
        type = "message", duration = 2
      )
    }
  })

  shiny::observeEvent(c(input$min_bins, input$max_bins), {
    if (input$auto_bin_range && !is.null(input$min_bins)) {
      shiny::updateCheckboxInput(session, "auto_bin_range", value = FALSE)
    }
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$run_binwidth_optimization, {
    shiny::req(shared_vis_data(), vis_results())

    tryCatch({
      shiny::withProgress(message = 'Optimizing binwidth...', {
        vis_data <- shared_vis_data()$data()
        vis_result <- vis_results()
        numeric_cols <- sapply(vis_data, is.numeric)
        excluded_cols <- c("x", "y", "color")
        available_cols <- names(vis_data)[numeric_cols]
        highd_cols <- setdiff(available_cols, excluded_cols)

        training_data <- vis_data[, highd_cols, drop = FALSE]
        new_col_names <- paste0("x", seq_along(highd_cols))
        names(training_data) <- new_col_names
        training_data$ID <- seq_len(nrow(training_data))
        embedding_data <- data.frame(
          EMBEDDING1 = vis_data$x,
          EMBEDDING2 = vis_data$y,
          ID = seq_len(nrow(vis_data))
        )
        x_range <- range(embedding_data$EMBEDDING1, na.rm = TRUE)
        y_range <- range(embedding_data$EMBEDDING2, na.rm = TRUE)
        x_span <- diff(x_range)
        y_span <- diff(y_range)

        embedding_scaled <- data.frame(
          EMBEDDING1 = (embedding_data$EMBEDDING1 - x_range[1]) / x_span,
          EMBEDDING2 = (embedding_data$EMBEDDING2 - y_range[1]) / y_span,
          ID = embedding_data$ID
        )
        bin_x_vec <- seq(input$min_bins, input$max_bins, by = 1)
        error_df_all <- data.frame()

        total_iterations <- length(bin_x_vec)

        for (i in seq_along(bin_x_vec)) {
          bin_x <- bin_x_vec[i]
          bin_y <- bin_x
          shiny::incProgress(1/total_iterations, detail = paste("Testing", bin_x, "x", bin_y, "bins"))
          tryCatch({
            model_result <- quollr::fit_highd_model(
              training_data = training_data,
              nldr_df_with_id = embedding_scaled,
              x = "EMBEDDING1",
              y = "EMBEDDING2",
              num_bins_x = bin_x,
              num_bins_y = bin_y,
              is_rm_lwd_hex = FALSE,
              col_start_2d = "EMBEDDING",
              col_start_highd = "x"
            )

            pred_result <- quollr::predict_emb(
              test_data = training_data,
              df_bin_centroids = model_result$df_bin_centroids,
              df_bin = model_result$df_bin,
              type_NLDR = vis_result$method
            )

            pred_df <- as.data.frame(do.call(cbind, pred_result))
            evaluation <- quollr::gen_summary(
              test_data = training_data,
              prediction_df = pred_df,
              df_bin = model_result$df_bin,
              col_start = "x"
            )

            error_row <- data.frame(
              bin_x = bin_x,
              bin_y = bin_y,
              total_bins = bin_x * bin_y,
              non_empty_bins = nrow(model_result$df_bin_centroids),
              MSE = evaluation$mse,
              AIC = evaluation$aic,
              binwidth_x = 1/bin_x,
              binwidth_y = 1/bin_y,
              method = vis_result$method,
              stringsAsFactors = FALSE
            )
            error_df_all <- rbind(error_df_all, error_row)

          }, error = function(e) {
            cat("Skipping bin configuration", bin_x, "x", bin_y, "- Error:", e$message, "\n")
          })
        }
        if (nrow(error_df_all) == 0) {
          stop("No valid bin configurations found")
        }
        optimal_row <- error_df_all[which.min(error_df_all$MSE), ]
        binwidth_optimization_results(error_df_all)
        optimal_config(optimal_row)

        shiny::showNotification(
          paste("Optimization completed! Optimal:", optimal_row$bin_x, "x", optimal_row$bin_y, "bins"),
          type = "message", duration = 3
        )
      })

    }, error = function(e) {
      shiny::showModal(shiny::modalDialog(
        title = "Binwidth Optimization Error",
        paste("Error during optimization:", e$message),
        easyClose = TRUE,
        footer = shiny::modalButton("OK")
      ))
    })
  })

  shiny::observeEvent(input$run_quollr_analysis, {
    shiny::req(shared_vis_data(), vis_results())
    if (is.null(optimal_config())) {
      shiny::showModal(shiny::modalDialog(
        title = "Optimization Required",
        "Please run binwidth optimization first to determine optimal configuration.",
        easyClose = TRUE,
        footer = shiny::modalButton("OK")
      ))
      return()
    }
    tryCatch({
      shiny::withProgress(message = 'Running Quollr Analysis...', {
        vis_data <- shared_vis_data()$data()
        vis_result <- vis_results()
        optimal <- optimal_config()
        cat("Total vis_data rows:", nrow(vis_data), "\n")
        cat("Total vis_data columns:", ncol(vis_data), "\n")
        cat("Column names:", paste(names(vis_data), collapse = ", "), "\n")
        cat("Using optimal configuration:", optimal$bin_x, "x", optimal$bin_y, "\n")
        numeric_cols <- sapply(vis_data, is.numeric)
        excluded_cols <- c("x", "y", "color")
        available_cols <- names(vis_data)[numeric_cols]
        highd_cols <- setdiff(available_cols, excluded_cols)
        if (length(highd_cols) == 0) {
          stop("No high-dimensional columns found. Need numeric columns beyond x, y coordinates.")
        }
        cat("High-dimensional columns found:", paste(highd_cols, collapse = ", "), "\n")
        cat("Number of high-d columns:", length(highd_cols), "\n")
        training_data <- vis_data[, highd_cols, drop = FALSE]
        new_col_names <- paste0("x", seq_along(highd_cols))
        names(training_data) <- new_col_names
        training_data$ID <- seq_len(nrow(training_data))
        cat("Training data dimensions:", nrow(training_data), "x", ncol(training_data), "\n")
        cat("Renamed columns:", paste(names(training_data), collapse = ", "), "\n")
        embedding_data <- data.frame(
          EMBEDDING1 = vis_data$x,
          EMBEDDING2 = vis_data$y,
          ID = seq_len(nrow(vis_data))
        )
        cat("Embedding data dimensions:", nrow(embedding_data), "x", ncol(embedding_data), "\n")
        if (any(is.na(embedding_data$EMBEDDING1)) || any(is.na(embedding_data$EMBEDDING2))) {
          stop("Missing values found in embedding coordinates")
        }
        if (any(!is.finite(embedding_data$EMBEDDING1)) || any(!is.finite(embedding_data$EMBEDDING2))) {
          stop("Infinite values found in embedding coordinates")
        }
        cat("Embedding data range - X:", range(embedding_data$EMBEDDING1), "Y:", range(embedding_data$EMBEDDING2), "\n")
        x_range <- range(embedding_data$EMBEDDING1, na.rm = TRUE)
        y_range <- range(embedding_data$EMBEDDING2, na.rm = TRUE)
        x_span <- diff(x_range)
        y_span <- diff(y_range)
        embedding_scaled <- data.frame(
          EMBEDDING1 = (embedding_data$EMBEDDING1 - x_range[1]) / x_span,
          EMBEDDING2 = (embedding_data$EMBEDDING2 - y_range[1]) / y_span,
          ID = embedding_data$ID
        )
        cat("Scaled embedding dimensions:", nrow(embedding_scaled), "x", ncol(embedding_scaled), "\n")
        cat("Scaled embedding range - X:", range(embedding_scaled$EMBEDDING1), "Y:", range(embedding_scaled$EMBEDDING2), "\n")
        if (nrow(embedding_scaled) == 0) {
          stop("Scaling resulted in empty dataset")
        }
        if (nrow(training_data) != nrow(embedding_scaled)) {
          stop(paste("Data dimension mismatch: training_data has", nrow(training_data),
                     "rows, embedding_scaled has", nrow(embedding_scaled), "rows"))
        }
        col_prefix <- "x"
        cat("Using column prefix:", col_prefix, "\n")
        cat("Starting model fitting with optimal bins:", optimal$bin_x, "x", optimal$bin_y, "\n")

        model_result <- quollr::fit_highd_model(
          training_data = training_data,
          nldr_df_with_id = embedding_scaled,
          x = "EMBEDDING1",
          y = "EMBEDDING2",
          num_bins_x = optimal$bin_x,
          num_bins_y = optimal$bin_y,
          is_rm_lwd_hex = input$quollr_remove_low_density,
          benchmark_to_rm_lwd_hex = if(input$quollr_remove_low_density) input$quollr_density_threshold else NA,
          col_start_2d = "EMBEDDING",
          col_start_highd = col_prefix
        )
        cat("Model fitting completed successfully\n")
        quollr_model(model_result)
        pred_result <- quollr::predict_emb(
          test_data = training_data,
          df_bin_centroids = model_result$df_bin_centroids,
          df_bin = model_result$df_bin,
          type_NLDR = vis_result$method
        )
        pred_df <- as.data.frame(do.call(cbind, pred_result))
        evaluation <- quollr::gen_summary(
          test_data = training_data,
          prediction_df = pred_df,
          df_bin = model_result$df_bin,
          col_start = col_prefix
        )
        quollr_results(list(
          model = model_result,
          predictions = pred_df,
          evaluation = evaluation,
          embedding_scaled = embedding_scaled,
          training_data = training_data,
          original_column_mapping = setNames(highd_cols, new_col_names),
          optimal_config_used = optimal
        ))
        shiny::showNotification("Quollr analysis completed successfully using optimal configuration!",
                                type = "message", duration = 3)
      })

    }, error = function(e) {
      cat("Detailed error information:\n")
      cat("Error message:", e$message, "\n")

      shiny::showModal(shiny::modalDialog(
        title = "Quollr Analysis Error",
        shiny::div(
          shiny::h4("Error Details:"),
          shiny::p(paste("Error message:", e$message)),
          shiny::hr(),
          shiny::h4("Potential Solutions:"),
          shiny::tags$ul(
            shiny::tags$li("Try running binwidth optimization with a different range"),
            shiny::tags$li("Check for extreme values in your embedding coordinates"),
            shiny::tags$li("Ensure your NLDR visualization completed successfully before running Quollr"),
            shiny::tags$li("Try with a smaller dataset to test the implementation")
          )
        ),
        easyClose = TRUE,
        footer = shiny::modalButton("OK")
      ))
    })
  })

  output$binwidth_mse_plot <- plotly::renderPlotly({
    shiny::req(binwidth_optimization_results())
    results <- binwidth_optimization_results()
    optimal <- optimal_config()
    p <- ggplot2::ggplot(results, ggplot2::aes(x = binwidth_x, y = MSE)) +
      ggplot2::geom_point(size = 2, alpha = 0.7, color = "steelblue") +
      ggplot2::geom_line(size = 0.5, color = "steelblue") +
      ggplot2::geom_point(data = optimal,
                          ggplot2::aes(x = binwidth_x, y = MSE),
                          color = "red", size = 4, shape = 21, fill = "red") +
      ggplot2::labs(
        title = "MSE vs Binwidth Optimization",
        x = "Binwidth (1/bins_x)",
        y = "Mean Squared Error (MSE)",
        caption = "Red point indicates optimal configuration"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 14, hjust = 0.5),
        axis.text = ggplot2::element_text(size = 10),
        axis.title = ggplot2::element_text(size = 12)
      )

    plotly::ggplotly(p, tooltip = c("x", "y")) %>%
      plotly::layout(
        annotations = list(
          x = optimal$binwidth_x, y = optimal$MSE,
          text = paste("Optimal:", optimal$bin_x, "x", optimal$bin_y),
          showarrow = TRUE, arrowhead = 4, arrowsize = .5, ax = 20, ay = -40
        )
      )
  })

  output$binwidth_results_table <- DT::renderDT({
    shiny::req(binwidth_optimization_results())
    results <- binwidth_optimization_results()
    display_results <- results %>%
      dplyr::arrange(MSE) %>%
      dplyr::mutate(
        MSE = round(MSE, 4),
        AIC = round(AIC, 2),
        binwidth_x = round(binwidth_x, 3)
      ) %>%
      dplyr::select(
        `Bins X` = bin_x,
        `Bins Y` = bin_y,
        `Total Bins` = total_bins,
        `Non-Empty Bins` = non_empty_bins,
        `Binwidth` = binwidth_x,
        `MSE` = MSE,
        `AIC` = AIC,
        `Method` = method
      )
    DT::datatable(
      display_results,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        order = list(list(5, 'asc'))
      ),
      rownames = FALSE
    ) %>%
      DT::formatStyle(
        "MSE",
        backgroundColor = DT::styleInterval(
          cuts = quantile(display_results$MSE, c(0.1, 0.9)),
          values = c("lightgreen", "white", "lightcoral")
        )
      )
  })

  output$quollr_fit_plot <- plotly::renderPlotly({
    shiny::req(quollr_results())
    results <- quollr_results()
    tryCatch({
      pred_data <- results$predictions
      pred_cols <- grep("pred_EMBEDDING", names(pred_data), value = TRUE)
      if (length(pred_cols) < 2) {
        pred_cols <- grep("pred_", names(pred_data), value = TRUE)[1:2]
      }
      if (length(pred_cols) >= 2) {
        pred_coords <- as.matrix(pred_data[, pred_cols[1:2]])
      } else {
        stop("Cannot find prediction coordinate columns")
      }
      original_coords <- as.matrix(results$embedding_scaled[, c("EMBEDDING1", "EMBEDDING2")])
      if (nrow(pred_coords) != nrow(original_coords)) {
        min_rows <- min(nrow(pred_coords), nrow(original_coords))
        pred_coords <- pred_coords[1:min_rows, , drop = FALSE]
        original_coords <- original_coords[1:min_rows, , drop = FALSE]
      }
      residuals <- sqrt(rowSums((pred_coords - original_coords)^2, na.rm = TRUE))
      plot_data <- data.frame(
        Original_X = original_coords[,1],
        Original_Y = original_coords[,2],
        Predicted_X = pred_coords[,1],
        Predicted_Y = pred_coords[,2],
        Residual = residuals
      )
      plot_data <- plot_data[complete.cases(plot_data), ]
      if (nrow(plot_data) == 0) {
        p <- ggplot2::ggplot() +
          ggplot2::annotate("text", x = 0.5, y = 0.5,
                            label = "No valid prediction data available") +
          ggplot2::theme_void()
      } else {
        p <- ggplot2::ggplot(plot_data) +
          ggplot2::geom_point(ggplot2::aes(x = Original_X, y = Original_Y, color = Residual),
                              size = 2.5, alpha = 0.75) +
          ggplot2::scale_color_viridis_c(name = "Prediction\nError") +
          ggplot2::labs(
            title = "Model Fit Quality",
            x = "Original 2D Embedding Dimension 1",
            y = "Original 2D Embedding Dimension 2"
          ) +
          ggplot2::coord_fixed(ratio = 1) +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            plot.title = ggplot2::element_text(size = 16, hjust = 0.5, margin = ggplot2::margin(b = 20)),
            axis.text = ggplot2::element_text(size = 11),
            axis.title = ggplot2::element_text(size = 13),
            axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 15)),
            axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 15)),
            legend.position = "right",
            panel.grid.minor = ggplot2::element_blank()
          )
      }

      overall_range <- range(c(plot_data$Original_X, plot_data$Original_Y), na.rm = TRUE)
      axis_padding <- diff(overall_range) * 0.05
      axis_min <- overall_range[1] - axis_padding
      axis_max <- overall_range[2] + axis_padding

      plotly::ggplotly(p) %>%
        plotly::layout(
          margin = list(l = 80, r = 50, t = 80, b = 80),
          autosize = TRUE,
          xaxis = list(
            title = list(standoff = 20),
            scaleanchor = "y",
            scaleratio = 1,
            range = c(axis_min, axis_max)
          ),
          yaxis = list(
            title = list(standoff = 25),
            range = c(axis_min, axis_max)
          )
        )

    }, error = function(e) {
      p <- ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0.5, y = 0.5,
                          label = paste("Error creating fit plot:", e$message),
                          size = 4, color = "red") +
        ggplot2::theme_void()
      plotly::ggplotly(p)
    })
  })

  output$current_config_summary <- shiny::renderText({
    optimal <- optimal_config()
    if (is.null(optimal)) {
      "No optimization run yet.\nRun binwidth optimization to configure analysis."
    } else {
      paste0(
        "Bins: ", optimal$bin_x, " × ", optimal$bin_y, "\n",
        "Remove low-density: ", input$quollr_remove_low_density, "\n",
        if (input$quollr_remove_low_density) {
          paste0("Density threshold: ", input$quollr_density_threshold)
        } else {
          "Density threshold: N/A"
        }
      )
    }
  })

  output$optimal_binwidth_summary <- shiny::renderPrint({
    shiny::req(optimal_config())

    optimal <- optimal_config()

    cat("Optimal Configuration\n")
    cat("====================\n")
    cat("Bins (X × Y):", optimal$bin_x, "×", optimal$bin_y, "\n")
    cat("Total bins:", optimal$total_bins, "\n")
    cat("Non-empty bins:", optimal$non_empty_bins, "\n")
    cat("Binwidth:", round(optimal$binwidth_x, 3), "\n")
    cat("MSE:", round(optimal$MSE, 4), "\n")
    cat("AIC:", round(optimal$AIC, 2), "\n")
  })

  output$quollr_model_summary <- shiny::renderPrint({
    shiny::req(quollr_results())
    results <- quollr_results()
    eval_metrics <- results$evaluation
    cat("Quollr Model Summary\n")
    cat("===================\n\n")
    if (!is.null(results$optimal_config_used)) {
      optimal <- results$optimal_config_used
      cat("Configuration Used:\n")
      cat("- Hexagonal bins (X × Y):", optimal$bin_x, "×", optimal$bin_y, "\n")
      cat("- Remove low-density hexagons:", input$quollr_remove_low_density, "\n")
      if (input$quollr_remove_low_density) {
        cat("- Density threshold:", input$quollr_density_threshold, "\n")
      }
      cat("- Number of non-empty bins:", nrow(results$model$df_bin_centroids), "\n\n")
    }

    cat("Model Performance:\n")
    cat("- Mean Squared Error (MSE):", round(eval_metrics$mse, 4), "\n")
    cat("- Akaike Information Criterion (AIC):", round(eval_metrics$aic, 4), "\n\n")

    cat("Data Summary:\n")
    cat("- Training observations:", nrow(results$training_data), "\n")
    cat("- High-dimensional features:", ncol(results$training_data) - 1, "\n")
  })
  shiny::observeEvent(input$show_langevitour, {
    shiny::req(quollr_results())
    show_langevitour_flag(TRUE)
  })
  output$show_langevitour_ui <- shiny::reactive({
    show_langevitour_flag()
  })
  shiny::outputOptions(output, "show_langevitour_ui", suspendWhenHidden = FALSE)
  shiny::observeEvent(input$run_quollr_analysis, {
    show_langevitour_flag(FALSE)
  })

   output$langevitour_output <- shiny::renderUI({
    shiny::req(show_langevitour_flag(), quollr_results())
    results <- quollr_results()
    tryCatch({
      df_clean <- results$training_data
      if ("ID" %in% names(df_clean)) {
        df_clean <- df_clean[, !names(df_clean) %in% "ID", drop = FALSE]
      }
      df_clean <- df_clean[complete.cases(df_clean), ]
      if (nrow(df_clean) == 0) {
        return(shiny::div(
          class = "alert alert-warning",
          "No complete cases available for 3D tour after removing missing values."
        ))
      }
      cat("Clean data for langevitour:", nrow(df_clean), "rows,", ncol(df_clean), "columns\n")
      df_bin_clean <- results$model$df_bin
      df_bin_clean <- df_bin_clean[complete.cases(df_bin_clean), ]
      df_centroids_clean <- results$model$df_bin_centroids
      df_centroids_clean <- df_centroids_clean[complete.cases(df_centroids_clean), ]
      col_prefix <- "x"
      tour_obj <- quollr::show_langevitour(
        df = df_clean,
        df_b = df_bin_clean,
        df_b_with_center_data = df_centroids_clean,
        col_start = col_prefix
      )
      shiny::div(
        style = "height: 400px; width: 100%;",
        tour_obj
      )
    }, error = function(e) {
      shiny::div(
        class = "alert alert-warning",
        shiny::h5("Unable to generate 3D tour"),
        shiny::p("Error details:", e$message),
        shiny::hr(),
        shiny::p("This may be due to:"),
        shiny::tags$ul(
          shiny::tags$li("Insufficient data points after cleaning"),
          shiny::tags$li("Missing values in high-dimensional data"),
          shiny::tags$li("Incompatible data structure for langevitour")
        )
      )
    })
  })
}
