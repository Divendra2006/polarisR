#' NLDR Visualization Tool Server
#'
#' Creates the server logic for the NLDR visualization tool
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @importFrom magrittr %>%
#' @return A Shiny server function
#' @keywords internal
nldr_viz_server <- function(input, output, session) {
  # Reactive value to store the current dataset
  dataset <- shiny::reactiveVal(NULL)

  # Reactive value to store the visualization results
  vis_results <- shiny::reactiveVal(NULL)

  # Track if Apply Changes was clicked
  apply_changes_clicked <- shiny::reactiveVal(FALSE)

  # Initialize custom datasets with those from data-raw folder
  custom_datasets <- shiny::reactiveVal(load_custom_datasets())

  # Reactive value to store available dataset choices
  available_datasets <- shiny::reactiveVal(c("None", "four_clusters", "pdfsense", "trees"))

  # Update example dataset choices when a new dataset is added
  shiny::observe({
    choices <- available_datasets()
    shiny::updateSelectInput(session, "example_data", choices = choices)
  })

  # Reset seed when button is clicked
  shiny::observeEvent(input$reset_seed, {
    new_seed <- sample(1:99999, 1)
    shiny::updateNumericInput(session, "seed", value = new_seed)
  })

  # Download settings
  output$download_settings <- shiny::downloadHandler(
    filename = function() {
      paste0("nldr_settings_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".json")
    },
    content = function(file) {
      # Collect all current settings
      settings <- list(
        nldr_method = input$nldr_method,
        seed = input$seed,
        color_auto = input$auto_color
      )

      # Add method-specific parameters
      if (input$nldr_method == "t-SNE") {
        settings$perplexity <- input$perplexity
        settings$max_iter <- input$max_iter_tsne
        settings$auto_perplexity <- input$auto_perplexity
      } else if (input$nldr_method == "UMAP") {
        settings$n_neighbors <- input$n_neighbors
        settings$min_dist <- input$min_dist
      }

      # Add color column if not auto
      if (!input$auto_color && !is.null(input$color_column)) {
        settings$color_column <- input$color_column
      }

      # Write settings to JSON file
      writeLines(jsonlite::toJSON(settings, pretty = TRUE), file)
    }
  )

  # Add uploaded dataset to examples
  shiny::observeEvent(input$add_to_examples, {
    shiny::req(dataset(), input$file)

    # Get dataset name (use file name if dataset_name is empty)
    dataset_name <- if (input$dataset_name != "") {
      input$dataset_name
    } else {
      tools::file_path_sans_ext(input$file$name)
    }

    # Check if the name already exists
    current_choices <- available_datasets()
    if (dataset_name %in% current_choices) {
      # Append a number to make the name unique
      i <- 1
      while(paste0(dataset_name, "_", i) %in% current_choices) {
        i <- i + 1
      }
      dataset_name <- paste0(dataset_name, "_", i)
    }

    # Add the dataset to custom_datasets
    current_custom <- custom_datasets()
    current_custom[[dataset_name]] <- dataset()
    custom_datasets(current_custom)

    # Update available datasets
    available_datasets(c(current_choices, dataset_name))

    # Show confirmation message
    shiny::showNotification(paste("Dataset", dataset_name, "added to example datasets"), type = "message")

    # Select the newly added dataset
    shiny::updateSelectInput(session, "example_data", selected = dataset_name)
  })

  # Load example datasets or user uploaded file
  shiny::observeEvent(input$example_data, {
    shiny::req(input$example_data != "None")

    # It's a custom dataset (either from data-raw or user-uploaded)
    custom_data_list <- custom_datasets()
    data <- custom_data_list[[input$example_data]]

    dataset(data)
    apply_changes_clicked(FALSE)
  })

  shiny::observeEvent(input$file, {
    file <- input$file
    shiny::req(file)

    # Check if file is CSV
    if(tools::file_ext(file$datapath) != "csv") {
      shiny::showNotification("Only CSV files are supported", type = "error")
      return()
    }

    # Read CSV file
    data <- read.csv(file$datapath, stringsAsFactors = TRUE)
    dataset(data)
    apply_changes_clicked(FALSE)
  })

  # Generate UI for column selection
  output$column_selection <- shiny::renderUI({
    shiny::req(dataset())
    data <- dataset()

    shiny::tagList(
      shiny::checkboxGroupInput("selected_columns", "Select columns for visualization:",
                                choices = names(data),
                                selected = if(input$auto_select) names(data)[sapply(data, is.numeric)] else NULL),
      shiny::conditionalPanel(
        condition = "input.apply_changes_clicked",
        shiny::actionButton("reset_columns", "Reset Column Selection", class = "btn-warning")
      )
    )
  })

  # Reset column selection when button is clicked
  shiny::observeEvent(input$reset_columns, {
    shiny::req(dataset())
    orig_data <- dataset()

    # Reset to original dataset with all columns
    if (!is.null(input$file)) {
      # Only read CSV file again
      file <- input$file
      orig_data <- read.csv(file$datapath, stringsAsFactors = TRUE)
    } else if (input$example_data != "None") {
      # It's a custom dataset
      custom_data_list <- custom_datasets()
      orig_data <- custom_data_list[[input$example_data]]
    }

    dataset(orig_data)
    shiny::updateCheckboxGroupInput(session, "selected_columns", selected = character(0))
    apply_changes_clicked(FALSE)
  })

  # Generate UI for color column selection
  output$color_column_selection <- shiny::renderUI({
    shiny::req(dataset())
    data <- dataset()

    # Prefer categorical columns for coloring
    categorical_cols <- names(data)[sapply(data, function(x) is.factor(x) || is.character(x))]
    if (length(categorical_cols) == 0) {
      categorical_cols <- names(data)
    }

    shiny::selectInput("color_column", "Color points by:",
                       choices = names(data),
                       selected = if(length(categorical_cols) > 0) categorical_cols[1] else names(data)[1])
  })

  # Update perplexity slider when auto_perplexity is toggled or when dataset changes
  shiny::observeEvent(c(input$auto_perplexity, dataset()), {
    shiny::req(dataset())
    data <- dataset()

    if (input$auto_perplexity) {
      perplexity_value <- min(30, floor(nrow(data) / 3) - 1)
      perplexity_value <- max(5, perplexity_value)
      shiny::updateSliderInput(session, "perplexity", value = perplexity_value)
    }
  })

  # Reset auto_perplexity when slider is manually changed
  shiny::observeEvent(input$perplexity, {
    # Only trigger when user manually changes the slider, not when it's programmatically updated
    if (input$auto_perplexity) {
      shiny::updateCheckboxInput(session, "auto_perplexity", value = FALSE)
    }
  }, ignoreInit = TRUE)

  # Update dataset based on column selection when Apply Changes is clicked
  shiny::observeEvent(input$apply_changes, {
    shiny::req(dataset())
    data <- dataset()

    if (!input$auto_select && !is.null(input$selected_columns)) {
      data <- data[, input$selected_columns, drop = FALSE]
    }

    dataset(data)
    apply_changes_clicked(TRUE)

    # Add notification when Apply Changes is clicked
    shiny::showNotification("Changes applied successfully!", type = "message", duration = 1.5)
  })

  # Display data preview
  output$data_preview <- DT::renderDT({
    shiny::req(dataset())
    DT::datatable(dataset(), options = list(scrollX = TRUE, pageLength = 10))
  })

  # Display dataset information
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

  # Run visualization when button is clicked
  shiny::observeEvent(input$run_visualization, {
    shiny::req(dataset())
    data <- dataset()

    # Select numeric columns for NLDR
    numeric_cols <- sapply(data, is.numeric)
    if (sum(numeric_cols) < 2) {
      shiny::showNotification("Need at least 2 numeric columns for dimensionality reduction", type = "error")
      return()
    }

    numeric_data <- data[, numeric_cols, drop = FALSE]

    # Ensure data is scaled
    scaled_data <- scale(numeric_data)

    # Handle missing values
    scaled_data[is.na(scaled_data)] <- 0

    # Get color column
    color_col <- if (input$auto_color) {
      # Prefer categorical columns
      categorical_cols <- names(data)[sapply(data, function(x) is.factor(x) || is.character(x))]
      if (length(categorical_cols) > 0) {
        categorical_cols[1]
      } else {
        names(data)[1]
      }
    } else {
      input$color_column
    }

    # Calculate optimal perplexity if auto-adjust is enabled
    perplexity_value <- input$perplexity
    if (input$auto_perplexity && input$nldr_method == "t-SNE") {
      perplexity_value <- min(30, floor(nrow(data) / 3) - 1)
      perplexity_value <- max(5, perplexity_value)
    }

    # Set seed for reproducibility
    set.seed(input$seed)

    # Run the selected NLDR method
    result <- list(
      method = input$nldr_method,
      color_col = color_col,
      color_values = data[[color_col]],
      seed = input$seed
    )

    # Run t-SNE or UMAP based on selection
    if (input$nldr_method == "t-SNE") {
      shiny::withProgress(message = 'Running t-SNE...', {
        # Check if perplexity is too large and cap it if needed (without changing the slider)
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

    # Store results for plotting
    vis_results(result)
  })

  # Generate the visualization plot
  output$nldr_plot <- plotly::renderPlotly({
    shiny::req(vis_results())
    result <- vis_results()

    # Create a data frame for plotting
    plot_data <- data.frame(
      x = result$coords[, 1],
      y = result$coords[, 2],
      color = result$color_values
    )

    # Generate the plot
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = x, y = y, color = color, text = color)) +
      ggplot2::geom_point(size = 3, alpha = 0.7) +
      ggplot2::labs(
        x = paste(result$method, "Dimension 1"),
        y = paste(result$method, "Dimension 2"),
        color = result$color_col) +
      ggplot2::coord_fixed(ratio = 1) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "right")

    plotly::layout(
      plotly::ggplotly(p, tooltip = "text"),
      autosize = TRUE,
      legend = list(title = list(text = result$color_col))
    )
  })

  # Display visualization information
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
}
