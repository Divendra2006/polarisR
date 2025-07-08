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
#' @return A Shiny server function
#' @keywords internal
nldr_viz_server <- function(input, output, session) {
  dataset <- shiny::reactiveVal(NULL)
  vis_results <- shiny::reactiveVal(NULL)
  apply_changes_clicked <- shiny::reactiveVal(FALSE)
  custom_datasets <- shiny::reactiveVal(load_custom_datasets())
  available_datasets <- shiny::reactiveVal(c("None", "four_clusters", "pdfsense", "trees"))

  nldr_datasets <- shiny::reactiveVal(list())
  shared_vis_data <- shiny::reactiveVal(NULL)
  color_palette <- shiny::reactiveVal(NULL)
  nldr_counter <- shiny::reactiveVal(0)

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

    if (isTRUE(input$enable_brushing)) {
      plotly::layout(
        plotly::highlight(plotly::ggplotly(p, tooltip = "text"),
                          on = "plotly_selected",
                          off = "plotly_doubleclick",
                          opacityDim = 0.2),
        autosize = TRUE,
        legend = list(title = list(text = result$color_col))
      )
    } else {
      plotly::layout(
        plotly::ggplotly(p, tooltip = "text"),
        autosize = TRUE,
        legend = list(title = list(text = result$color_col))
      )
    }
  })

  output$nldr_plot <- plotly::renderPlotly({
    nldr_plotly_object()
  })

  output$nldr_plot_tour_tab <- plotly::renderPlotly({
    nldr_plotly_object()
  })

  output$dynamic_tour_output_ui <- shiny::renderUI({
    shiny::req(
      shared_vis_data(),
      input$tour_display_type
    )
    detourr::displayScatter2dOutput("tour_plot_2d", height = "550px")
  })

  tour_object <- shiny::reactive({
    shiny::req(
      shared_vis_data(),
      input$tour_display_type,
      !is.null(input$tour_axes),
      color_palette()
    )

    sd_obj <- shared_vis_data()
    pal <- color_palette()
    all_cols <- names(sd_obj$data())
    projection_cols <- all_cols[!all_cols %in% c("x", "y", "color")]

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
        detour_obj |> detourr::show_scatter(alpha = input$tour_alpha, axes = input$tour_axes, palette = pal, size = 1)
      },
      "Sage" = {
        shiny::req(input$tour_gamma)
        detour_obj |> detourr::show_sage(gamma = input$tour_gamma, axes = input$tour_axes, palette = pal, size = 1)
      },
      "Slice" = {
        shiny::req(input$tour_slice_volume)
        detour_obj |> detourr::show_slice(slice_relative_volume = input$tour_slice_volume, axes = input$tour_axes, palette = pal, size = 1)
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
}
