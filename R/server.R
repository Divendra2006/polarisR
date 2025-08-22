#' NLDR Visualization Tool Server Logic
#'
#' Implements the comprehensive server-side logic for the NLDR visualization tool.
#' This function handles all reactive computations, data processing, visualization
#' generation, and user interactions for the multi-tab NLDR analysis application.
#'
#' @details 
#' The server function manages several key areas of functionality:
#' 
#' **Data Management:**
#' \itemize{
#'   \item File upload validation and CSV parsing with error handling
#'   \item Built-in dataset loading (four_clusters, pdfsense, fake_trees)
#'   \item Empty cell detection and data quality validation
#'   \item Column selection and filtering capabilities
#'   \item Dynamic dataset storage and retrieval system
#' }
#' 
#' **NLDR Computations:**
#' \itemize{
#'   \item t-SNE implementation with parameter validation and auto-adjustment
#'   \item UMAP processing with neighbor and distance parameter controls
#'   \item Asynchronous computation using future package for responsiveness
#'   \item Progress tracking and user feedback during long computations
#'   \item Result caching and session management
#' }
#' 
#' **Interactive Visualizations:**
#' \itemize{
#'   \item Plotly-based interactive scatter plots with zoom, pan, and selection
#'   \item Color mapping with automatic palette generation
#'   \item Linked brushing across multiple visualizations
#'   \item Responsive plot sizing and layout management
#'   \item Hover tooltips and selection feedback
#' }
#' 
#' **Dynamic Tours:**
#' \itemize{
#'   \item Integration with detourr package for animated projections
#'   \item Multiple display types: Scatter, Sage, and Slice projections
#'   \item 5-nearest neighbor graph construction and visualization
#'   \item Real-time parameter adjustment and tour customization
#'   \item Coordinated views between static and dynamic visualizations
#' }
#' 
#' **Quality Assessment (Quollr Integration):**
#' \itemize{
#'   \item Automated binwidth optimization using RMSE minimization
#'   \item Hexagonal binning and centroid extraction
#'   \item High-dimensional model fitting and validation
#'   \item Prediction error analysis and visualization
#'   \item 3D model tours using langevitour integration
#' }
#' 
#' **Method Comparison:**
#' \itemize{
#'   \item Side-by-side visualization comparison with linked brushing
#'   \item RMSE-based parameter optimization comparison
#'   \item Best configuration identification and reporting
#'   \item Interactive comparison plot generation
#' }
#' 
#' **State Management:**
#' \itemize{
#'   \item Reactive value system for maintaining application state
#'   \item Session-based data persistence and cleanup
#'   \item Asynchronous operation tracking and user feedback
#'   \item Memory management for large datasets
#' }
#' 
#' **Parallel Processing:**
#' The server automatically configures parallel processing:
#' \itemize{
#'   \item Uses multicore on supported systems, multisession otherwise
#'   \item Configures 2 worker processes for optimal performance
#'   \item Proper cleanup on session end to prevent memory leaks
#'   \item Future-based asynchronous computation for UI responsiveness
#' }
#'
#' @param input The Shiny input object containing all user interface inputs.
#'   This includes form controls, button clicks, plot selections, and file uploads.
#' @param output The Shiny output object for sending rendered content to the UI.
#'   Used for plots, tables, text outputs, and dynamic UI elements.
#' @param session The Shiny session object for managing client-server communication.
#'   Provides access to session state, input updates, and client information.
#'
#' @return Invisible NULL. The function sets up reactive expressions and observers
#'   that handle all server-side logic. The actual outputs are managed through
#'   the Shiny reactive system and sent to the client via the output object.
#'
#' @section Reactive Values:
#' The server maintains several key reactive values:
#' \itemize{
#'   \item \code{dataset}: Current active dataset
#'   \item \code{vis_results}: NLDR computation results
#'   \item \code{shared_vis_data}: Crosstalk-enabled data for linked brushing
#'   \item \code{nldr_datasets}: Storage for multiple NLDR results
#'   \item \code{optimal_config}: Best binwidth configuration from optimization
#'   \item \code{quollr_results}: Quality assessment results
#'   \item \code{color_palette}: Current color scheme for visualizations
#'   \item \code{is_running_*}: Boolean flags for operation status tracking
#' }
#'
#' @section Error Handling:
#' Comprehensive error handling includes:
#' \itemize{
#'   \item File upload validation with user-friendly error messages
#'   \item NLDR computation error catching with fallback options
#'   \item Memory management for large datasets
#'   \item Network timeout handling for async operations
#'   \item Graceful degradation when optional features are unavailable
#' }
#'
#' @section Performance Considerations:
#' \itemize{
#'   \item Asynchronous computations prevent UI blocking
#'   \item Efficient data structures for large datasets
#'   \item Caching of expensive computations
#'   \item Memory cleanup and garbage collection
#'   \item Optimized reactive dependency management
#' }
#'
#' @note 
#' This function requires several packages to be available:
#' \itemize{
#'   \item **Core**: shiny, magrittr
#'   \item **Visualization**: plotly, ggplot2, DT, scales
#'   \item **NLDR**: Rtsne, umap, FNN
#'   \item **Quality**: quollr (with all its dependencies)
#'   \item **Tours**: detourr, tourr  
#'   \item **Data**: dplyr, crosstalk
#'   \item **Async**: future
#'   \item **Utils**: stats, utils, tools
#' }
#'
#' @seealso 
#' \itemize{
#'   \item \code{\link{nldr_viz_ui}} for the corresponding user interface
#'   \item \code{\link{run_nldr_viz}} for launching the complete application
#'   \item \code{\link{load_custom_datasets}} for data loading utilities
#'   \item \code{\link[shiny]{shinyServer}} for Shiny server function details
#'   \item \code{\link[future]{plan}} for parallel processing configuration
#' }
#'
#' @import shiny
#' @importFrom magrittr %>%
#' @importFrom crosstalk SharedData
#' @importFrom plotly highlight renderPlotly plotlyOutput ggplotly layout config plot_ly add_text
#' @importFrom FNN get.knn
#' @importFrom future future resolved value plan multisession multicore sequential supportsMulticore
#' @importFrom ggplot2 ggplot aes geom_point labs coord_fixed theme_minimal geom_line scale_color_manual guide_legend
#' @importFrom DT renderDT datatable DTOutput
#' @importFrom detourr detour tour_aes tour_path show_scatter show_sage show_slice shinyRenderDisplayScatter2d displayScatter2dOutput
#' @importFrom tourr grand_tour
#' @importFrom quollr gen_scaled_data hex_binning extract_hexbin_centroids tri_bin_centroids gen_edges avg_highd_data gen_diffbin1_errors predict_emb comb_data_model show_langevitour glance
#' @importFrom dplyr filter group_by slice_min ungroup arrange left_join mutate bind_rows
#' @importFrom stats quantile setNames var
#' @importFrom utils data read.csv
#' @export
#'
#' @examples
#' \dontrun{
#' # Use in a complete Shiny application
#' shinyApp(
#'   ui = nldr_viz_ui(),
#'   server = nldr_viz_server
#' )
#' 
#' # Test server logic (useful for development)
#' testServer(nldr_viz_server, {
#'   # Test reactive values and functionality
#'   session$setInputs(example_data = "four_clusters")
#'   expect_true(!is.null(dataset()))
#' })
#' }
#'
#' @author GSoC Contributor
#' @keywords shiny server reactive dimensionality-reduction visualization
nldr_viz_server <- function(input, output, session) {
 
  x <- y <- color <- n_h <- h <- b1 <- a1 <- RMSE <- NULL
  emb1 <- pred_emb_1 <- emb2 <- pred_emb_2 <- Residual <- NULL
  Error_Level <- tooltip_text <- NULL
  
  if (!future::supportsMulticore()) {
    future::plan(future::multisession, workers = 2)
  } else {
    future::plan(future::multicore, workers = 2)
  }

  session$onSessionEnded(function() {
    future::plan(future::sequential)
  })

  dataset <- shiny::reactiveVal(NULL)
  vis_results <- shiny::reactiveVal(NULL)
  apply_changes_clicked <- shiny::reactiveVal(FALSE)
  custom_datasets <- shiny::reactiveVal(load_custom_datasets())
  available_datasets <- shiny::reactiveVal(c("None", "four_clusters", "pdfsense", "trees"))

  quollr_model <- shiny::reactiveVal(NULL)
  quollr_results <- shiny::reactiveVal(NULL)
  is_running_visualization <- shiny::reactiveVal(FALSE)
  is_running_binwidth_optimization <- shiny::reactiveVal(FALSE)
  is_running_quollr_analysis <- shiny::reactiveVal(FALSE)
  is_running_comparison <- shiny::reactiveVal(FALSE)
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

  output$visualization_button_disabled <- shiny::reactive({
    is_running_visualization()
  })
  shiny::outputOptions(output, "visualization_button_disabled", suspendWhenHidden = FALSE)

  output$binwidth_button_disabled <- shiny::reactive({
    is_running_binwidth_optimization()
  })
  shiny::outputOptions(output, "binwidth_button_disabled", suspendWhenHidden = FALSE)

  output$quollr_button_disabled <- shiny::reactive({
    is_running_quollr_analysis()
  })
  shiny::outputOptions(output, "quollr_button_disabled", suspendWhenHidden = FALSE)

  output$comparison_button_disabled <- shiny::reactive({
    is_running_comparison()
  })
  shiny::outputOptions(output, "comparison_button_disabled", suspendWhenHidden = FALSE)

  # Extract base dataset name from full display name
  # Internal helper function for identifying original dataset names
  extract_base_dataset_name <- function(full_name) {
    if (grepl("\\s-\\s", full_name)) {
      base_name <- gsub("\\s*-\\s*(t-SNE|UMAP).*$", "", full_name)
      return(trimws(base_name))
    } else {
      return(full_name)
    }
  }

  # Check for empty cells in dataset
  # Internal helper function for data quality validation
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
    data <- dataset()
    numeric_cols <- sapply(data, is.numeric)
    data[numeric_cols] <- lapply(data[numeric_cols], function(x) round(x, 3))
    DT::datatable(data, options = list(scrollX = TRUE, pageLength = 10))
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

    if (is_running_visualization()) return()
    is_running_visualization(TRUE)

    tryCatch(
      {
        empty_check <- check_empty_cells(data)
        if (empty_check$has_empty) {
          shiny::showModal(shiny::modalDialog(title = "Cannot Run", "The dataset contains empty cells."))
          is_running_visualization(FALSE)
          return()
        }

        numeric_cols_idx <- sapply(data, is.numeric)
        if (sum(numeric_cols_idx) < 2) {
          shiny::showNotification("Need at least 2 numeric columns for NLDR", type = "error")
          is_running_visualization(FALSE)
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
          is_running_visualization(FALSE)
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
        method <- input$nldr_method
        max_iter_value <- input$max_iter_tsne
        n_neighbors_value <- input$n_neighbors
        min_dist_value <- input$min_dist
        computation_promise <- if (method == "t-SNE") {
          future::future({
            max_allowed_perplexity <- (nrow(data) - 1) / 3
            if (perplexity_value >= max_allowed_perplexity) {
              perplexity_value <- floor(max_allowed_perplexity)
            }
            tsne_result <- Rtsne::Rtsne(
              scaled_data,
              dims = 2,
              perplexity = perplexity_value,
              max_iter = max_iter_value,
              check_duplicates = FALSE,
              pca = TRUE,
              verbose = FALSE
            )
            list(
              coords = tsne_result$Y,
              perplexity = perplexity_value,
              max_iter = max_iter_value,
              method = "t-SNE"
            )
          }, seed = TRUE)
        } else {
          future::future({
            umap_config <- umap::umap.defaults
            umap_config$n_neighbors <- n_neighbors_value
            umap_config$min_dist <- min_dist_value
            umap_result <- umap::umap(scaled_data, config = umap_config)
            list(
              coords = umap_result$layout,
              n_neighbors = n_neighbors_value,
              min_dist = min_dist_value,
              method = "UMAP"
            )
          }, seed = TRUE)
        }

        shiny::withProgress(message = paste("Running", method, "visualization..."), value = 0, {
          shiny::incProgress(0.1, detail = "Validating parameters...")
          Sys.sleep(0.1)
          shiny::incProgress(0.1, detail = paste("Computing", method, "embedding..."))
          progress_steps <- seq(0.2, 0.7, length.out = 10)
          for (i in seq_along(progress_steps)) {
            if (future::resolved(computation_promise)) break
            Sys.sleep(0.3)
            shiny::incProgress(0.05, detail = "Processing...")
          }
          computation_result <- future::value(computation_promise)

          result$coords <- computation_result$coords
          if (computation_result$method == "t-SNE") {
            result$perplexity <- computation_result$perplexity
            result$max_iter <- computation_result$max_iter
            if (computation_result$perplexity != perplexity_value) {
              shiny::showNotification(paste("Perplexity adjusted to", computation_result$perplexity), type = "warning")
            }
          } else {
            result$n_neighbors <- computation_result$n_neighbors
            result$min_dist <- computation_result$min_dist
          }

          shiny::incProgress(0.3, detail = "Preparing visualization...")
        })

        shiny::withProgress(message = "Finalizing results...", value = 0.8, {
          vis_results(result)
          color_as_factor <- as.factor(result$color_values)
          pal <- scales::hue_pal()(length(levels(color_as_factor)))
          names(pal) <- levels(color_as_factor)
          color_palette(pal)
          plot_data_for_shared <- data.frame(x = result$coords[, 1], y = result$coords[, 2], color = result$color_values)
          plot_data_for_shared <- cbind(plot_data_for_shared, as.data.frame(scaled_data))
          shared_vis_data(crosstalk::SharedData$new(plot_data_for_shared, key = ~ row.names(plot_data_for_shared)))
          id <- nldr_counter() + 1
          nldr_counter(id)
          active_nldr_id(as.character(id))
          method_settings <- if (method == "t-SNE") {
            paste0(current_dataset_name(), " - t-SNE (p=", result$perplexity, ", iter=", result$max_iter, ")")
          } else {
            paste0(current_dataset_name(), " - UMAP (n=", result$n_neighbors, ", d=", result$min_dist, ")")
          }
          current <- nldr_datasets()
          current[[as.character(id)]] <- list(
            id = id, 
            name = method_settings, 
            result = result, 
            tour_input_data = plot_data_for_shared, 
            color_palette = pal,
            timestamp = Sys.time()
          )
          nldr_datasets(current)
          shiny::incProgress(0.2, detail = "Complete!")
        })

        quollr_results(NULL)
        optimal_config(NULL)
        is_running_visualization(FALSE)
        shiny::showNotification("Visualization completed successfully!", type = "message")
      },
      error = function(e) {
        is_running_visualization(FALSE)
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

  

  tour_object <- shiny::reactive({
    shiny::req(shared_vis_data(), input$tour_display_type, !is.null(input$tour_axes), color_palette())
    
    brushing_enabled <- input$enable_brushing
    
    sd_obj <- shared_vis_data()
    stored_pal <- color_palette()
    projection_cols <- setdiff(names(sd_obj$data()), c("x", "y", "color"))

    if (length(projection_cols) < 2) {
      shiny::showNotification("Not enough high-dimensional columns for the tour.", type = "warning")
      return(NULL)
    }

    current_color_levels <- as.factor(sd_obj$data()$color)
    if (length(stored_pal) != length(levels(current_color_levels))) {
      pal <- scales::hue_pal()(length(levels(current_color_levels)))
      names(pal) <- levels(current_color_levels)
    } else {
      if (!all(names(stored_pal) %in% levels(current_color_levels))) {
        pal <- scales::hue_pal()(length(levels(current_color_levels)))
        names(pal) <- levels(current_color_levels)
      } else {
        pal <- stored_pal
      }
    }

    detour_obj <- detourr::detour(
      sd_obj,
      detourr::tour_aes(projection = projection_cols, colour = color)
    ) |>
      detourr::tour_path(tourr::grand_tour(2L), fps = 30)

    switch(input$tour_display_type,
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
    if (length(datasets) == 0) return()
    
    for (ds_id in names(datasets)) {
      checkbox_input <- input[[paste0("nldr_", ds_id)]]
      if (isTRUE(checkbox_input)) {
        ds <- datasets[[ds_id]]
        active_nldr_id(ds_id)
        vis_results(ds$result)
        shared_vis_data(crosstalk::SharedData$new(ds$tour_input_data, key = ~ row.names(ds$tour_input_data)))
        
        if (!is.null(ds$color_palette)) {
          color_palette(ds$color_palette)
        } else {
          color_as_factor <- as.factor(ds$result$color_values)
          pal <- scales::hue_pal()(length(levels(color_as_factor)))
          names(pal) <- levels(color_as_factor)
          color_palette(pal)
        }
        
        shiny::showNotification(paste("Loaded:", ds$name), type = "message")
        shiny::updateCheckboxInput(session, paste0("nldr_", ds_id), value = FALSE)
        break  
      }
    }
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
    if (is_running_quollr_analysis()) return()
    is_running_quollr_analysis(TRUE)
    tryCatch(
      {
        shiny::withProgress(message = "Building Quollr Model...", value = 0, {
          shiny::incProgress(0.1, detail = "Preparing data...")
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

          shiny::incProgress(0.2, detail = "Finalizing model...")
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

        })

        is_running_quollr_analysis(FALSE)
        shiny::showNotification("Quollr analysis completed successfully!", type = "message")
      },
      error = function(e) {
        is_running_quollr_analysis(FALSE)
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

    if (is_running_binwidth_optimization()) return()
    is_running_binwidth_optimization(TRUE)

    tryCatch(
      {
        vis_data <- shared_vis_data()$data()
        highd_cols <- setdiff(names(vis_data)[sapply(vis_data, is.numeric)], c("x", "y", "color"))
        highd_data <- vis_data[, highd_cols, drop = FALSE]
        highd_data$ID <- seq_len(nrow(highd_data))
        nldr_data <- data.frame(emb1 = vis_data$x, emb2 = vis_data$y, ID = seq_len(nrow(vis_data)))
        optimization_promise <- future::future({
          error_df_all <- quollr::gen_diffbin1_errors(highd_data = highd_data, nldr_data = nldr_data)
          if (is.null(error_df_all) || nrow(error_df_all) == 0) {
            stop("Optimization failed.")
          }

          processed_results <- error_df_all %>%
            dplyr::filter(b1 >= 5) %>%
            dplyr::group_by(a1) %>%
            dplyr::slice_min(RMSE, n = 1, with_ties = FALSE) %>%
            dplyr::ungroup() %>%
            dplyr::arrange(a1)

          optimal_row <- processed_results %>% dplyr::slice_min(RMSE, n = 1)

          list(
            processed_results = processed_results,
            optimal_row = optimal_row
          )
        }, seed = TRUE)

        shiny::withProgress(message = "Optimizing binwidth...", value = 0, {
          shiny::incProgress(0.1, detail = "Preparing data...")
          Sys.sleep(0.1)  # Brief pause for UI update

          shiny::incProgress(0.2, detail = "Computing optimization errors...")

          progress_steps <- seq(0.3, 0.7, length.out = 8)
          for (i in seq_along(progress_steps)) {
            if (future::resolved(optimization_promise)) break
            Sys.sleep(0.4)
            shiny::incProgress(0.05, detail = paste("Analyzing binwidth configuration", i, "..."))
          }

          optimization_result <- future::value(optimization_promise)

          shiny::incProgress(0.2, detail = "Processing results...")

          shiny::req(active_nldr_id())
          current_vis_id <- active_nldr_id()

          all_stored_results <- binwidth_optimization_results()
          all_stored_results[[current_vis_id]] <- optimization_result$processed_results
          binwidth_optimization_results(all_stored_results)
          optimal_config(optimization_result$optimal_row)

          shiny::incProgress(0.1, detail = "Complete!")
        })

        is_running_binwidth_optimization(FALSE)
        shiny::showNotification("Binwidth optimization completed successfully!", type = "message")
      },
      error = function(e) {
        is_running_binwidth_optimization(FALSE)
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
    numeric_cols <- sapply(results_for_plot, is.numeric)
    results_for_plot[numeric_cols] <- lapply(results_for_plot[numeric_cols], function(x) round(x, 3))
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
      dplyr::mutate(
        Residual = sqrt((emb1 - pred_emb_1)^2 + (emb2 - pred_emb_2)^2),
       Error_Level = cut(
          Residual, 
          breaks = quantile(Residual, probs = c(0, 0.33, 0.67, 1), na.rm = TRUE),
          labels = c("Low Error", "Medium Error", "High Error"),
          include.lowest = TRUE
        ),
        tooltip_text = paste(
          "Embedding 1:", round(emb1, 3), "<br>",
          "Embedding 2:", round(emb2, 3), "<br>",
          "Prediction Error:", round(Residual, 4), "<br>",
          "Error Level:", Error_Level
        )
      )

    p <- ggplot2::ggplot(plot_data, ggplot2::aes(
      x = emb1, 
      y = emb2, 
      color = Error_Level,
      text = tooltip_text
    )) +
      ggplot2::geom_point(
        size = 2.2, 
        alpha = 0.8,
        stroke = 0
      ) +
      ggplot2::scale_color_manual(
        values = c(
          "Low Error" = "#66B2CC",    
          "Medium Error" = "#FFDD88", 
          "High Error" = "#FF7755"    
        ),
        name = "Model Fit Quality",
        guide = ggplot2::guide_legend(
          title.position = "top",
          title.hjust = 0.5,
          override.aes = list(size = 4, alpha = 1)
        )
      ) +
      ggplot2::coord_fixed(ratio = 1) +
      ggplot2::labs(
        title = "Model Fit Quality: Prediction Error Distribution",
        subtitle = "Blue: Good fit | Orange: Medium fit | Red: Poor fit",
        x = "Embedding Dimension 1",
        y = "Embedding Dimension 2"
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = ggplot2::element_text(size = 11, hjust = 0.5, color = "gray40"),
        legend.position = "right",
        legend.title = ggplot2::element_text(size = 11, face = "bold"),
        legend.text = ggplot2::element_text(size = 10),
        panel.grid.minor = ggplot2::element_blank(),
        panel.background = ggplot2::element_rect(fill = "white", color = NA),
        plot.background = ggplot2::element_rect(fill = "white", color = NA)
      )

    plotly::ggplotly(p, tooltip = "text") %>%
      plotly::layout(
        showlegend = TRUE,
        margin = list(l = 50, r = 100, t = 80, b = 50)
      ) %>%
      plotly::config(displayModeBar = TRUE, displaylogo = FALSE)
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

    if (is_running_comparison()) return()
    is_running_comparison(TRUE)

    tryCatch(
      {
        shiny::withProgress(message = "Generating comparison...", value = 0, {
          shiny::incProgress(0.1, detail = "Collecting optimization results...")
          Sys.sleep(0.1)  # Brief pause for UI update

          stored_results <- binwidth_optimization_results()
          datasets_info <- nldr_datasets()
          results_to_compare <- list()

          shiny::incProgress(0.3, detail = "Processing selected datasets...")
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

          shiny::incProgress(0.4, detail = "Combining results...")
          if (length(results_to_compare) > 0) {
            comparison_results(dplyr::bind_rows(results_to_compare))
            shiny::incProgress(0.2, detail = "Complete!")
          }
        })

        is_running_comparison(FALSE)
        shiny::showNotification("Comparison analysis completed successfully!", type = "message")
      },
      error = function(e) {
        is_running_comparison(FALSE)
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

    cat("*** Best Configuration Found ***\n--------------------------\n")
    cat("Name:", overall_best$dataset_name, "\n")
    cat("Method:", overall_best$method, "\n")
    cat("Optimal Binwidth (a1):", round(overall_best$a1, 3), "\n")
    cat("Corresponding Bins (b1):", overall_best$b1, "\n")
    cat("Resulting RMSE:", round(overall_best$RMSE, 5), "\n")
  })

  sidebyside_dataset1 <- shiny::reactiveVal(NULL)
  sidebyside_dataset2 <- shiny::reactiveVal(NULL)
  sidebyside_shared_data1 <- shiny::reactiveVal(NULL)
  sidebyside_shared_data2 <- shiny::reactiveVal(NULL)

  output$sidebyside_dataset1_selection <- shiny::renderUI({
    datasets <- nldr_datasets()
    if (length(datasets) == 0) {
      return(shiny::p("No NLDR visualizations available. Create visualizations first."))
    }
    
    choices <- setNames(names(datasets), sapply(datasets, function(ds) ds$name))
    shiny::selectInput("sidebyside_dataset1", "Select First Dataset:",
      choices = c("Select a dataset..." = "", choices),
      selected = ""
    )
  })

  output$sidebyside_dataset2_selection <- shiny::renderUI({
    datasets <- nldr_datasets()
    if (length(datasets) == 0) {
      return(NULL) 
    }
    
    choices <- setNames(names(datasets), sapply(datasets, function(ds) ds$name))
    shiny::selectInput("sidebyside_dataset2", "Select Second Dataset:",
      choices = c("Select a dataset..." = "", choices),
      selected = ""
    )
  })

  shiny::observeEvent(input$generate_sidebyside_comparison, {
    shiny::req(input$sidebyside_dataset1, input$sidebyside_dataset2)
    
    if (input$sidebyside_dataset1 == "" || input$sidebyside_dataset2 == "") {
      shiny::showNotification("Please select both datasets for comparison.", type = "warning")
      return()
    }
    
    if (input$sidebyside_dataset1 == input$sidebyside_dataset2) {
      shiny::showNotification("Please select two different datasets for comparison.", type = "warning")
      return()
    }
    
    datasets <- nldr_datasets()
    dataset1 <- datasets[[input$sidebyside_dataset1]]
    dataset2 <- datasets[[input$sidebyside_dataset2]]
    
    if (nrow(dataset1$result$coords) != nrow(dataset2$result$coords)) {
      shiny::showNotification("Warning: Datasets have different numbers of points. Linked brushing may not work as expected.", type = "warning")
    }
    
    sidebyside_dataset1(dataset1)
    sidebyside_dataset2(dataset2)

    if (input$enable_linked_brushing) {
      n_points <- min(nrow(dataset1$result$coords), nrow(dataset2$result$coords))
      shared_keys <- paste0("point_", seq_len(n_points))

      plot_data1 <- data.frame(
        x = dataset1$result$coords[1:n_points, 1],
        y = dataset1$result$coords[1:n_points, 2],
        color = dataset1$result$color_values[1:n_points],
        dataset = "Dataset 1",
        key = shared_keys,
        stringsAsFactors = FALSE
      )
      
      plot_data2 <- data.frame(
        x = dataset2$result$coords[1:n_points, 1],
        y = dataset2$result$coords[1:n_points, 2],
        color = dataset2$result$color_values[1:n_points],
        dataset = "Dataset 2",
        key = shared_keys,
        stringsAsFactors = FALSE
      )

      shared1 <- crosstalk::SharedData$new(plot_data1, key = ~key, group = "sidebyside_comparison")
      shared2 <- crosstalk::SharedData$new(plot_data2, key = ~key, group = "sidebyside_comparison")
      
      sidebyside_shared_data1(shared1)
      sidebyside_shared_data2(shared2)
    } else {
      sidebyside_shared_data1(NULL)
      sidebyside_shared_data2(NULL)
    }
    
    shiny::showNotification("Side-by-side comparison generated successfully!", type = "message")
  })

  shiny::observeEvent(input$clear_sidebyside_selection, {
    shiny::updateSelectInput(session, "sidebyside_dataset1", selected = "")
    shiny::updateSelectInput(session, "sidebyside_dataset2", selected = "")
    sidebyside_dataset1(NULL)
    sidebyside_dataset2(NULL)
    sidebyside_shared_data1(NULL)
    sidebyside_shared_data2(NULL)
  })

  output$sidebyside_plot1_title <- shiny::renderText({
    dataset1 <- sidebyside_dataset1()
    if (is.null(dataset1)) return("First Dataset")
    return(dataset1$name)
  })

  output$sidebyside_plot2_title <- shiny::renderText({
    dataset2 <- sidebyside_dataset2()
    if (is.null(dataset2)) return("Second Dataset")
    return(dataset2$name)
  })

  output$sidebyside_plot1 <- plotly::renderPlotly({
    dataset1 <- sidebyside_dataset1()
    if (is.null(dataset1)) {
      return(plotly::plot_ly() %>% 
        plotly::add_text(x = 0.5, y = 0.5, text = "Select datasets to compare", 
                        textfont = list(size = 16, color = "gray")) %>%
        plotly::layout(xaxis = list(visible = FALSE), yaxis = list(visible = FALSE)))
    }
 
    color_pal <- dataset1$color_palette
    if (is.null(color_pal)) {
      color_as_factor <- as.factor(dataset1$result$color_values)
      color_pal <- scales::hue_pal()(length(levels(color_as_factor)))
      names(color_pal) <- levels(color_as_factor)
    }
    
    if (input$enable_linked_brushing && !is.null(sidebyside_shared_data1())) {
      plot_data <- sidebyside_shared_data1()
 
      p <- plot_data %>%
        plotly::plot_ly(x = ~x, y = ~y, color = ~color,
                       colors = color_pal,
                       type = "scatter", mode = "markers",
                       marker = list(size = 6, opacity = 0.7),
                       hovertemplate = "<b>%{color}</b><br>X: %{x:.3f}<br>Y: %{y:.3f}<extra></extra>",
                       source = "sidebyside_plot1") %>%
        plotly::layout(
          title = list(text = dataset1$name, font = list(size = 14)),
          xaxis = list(title = paste(dataset1$result$method, "Dimension 1")),
          yaxis = list(title = paste(dataset1$result$method, "Dimension 2")),
          showlegend = TRUE,
          legend = list(title = list(text = dataset1$result$color_col)),
          dragmode = "select"
        ) %>%
        plotly::highlight(
          on = "plotly_selected", 
          off = "plotly_deselect",
          opacityDim = 0.3,
          selected = plotly::attrs_selected(opacity = 1)
        )
    } else {
      p <- plotly::plot_ly(x = dataset1$result$coords[, 1], 
                          y = dataset1$result$coords[, 2],
                          color = dataset1$result$color_values,
                          colors = color_pal,
                          type = "scatter", mode = "markers",
                          marker = list(size = 6, opacity = 0.7),
                          hovertemplate = "<b>%{color}</b><br>X: %{x:.3f}<br>Y: %{y:.3f}<extra></extra>") %>%
        plotly::layout(
          title = list(text = dataset1$name, font = list(size = 14)),
          xaxis = list(title = paste(dataset1$result$method, "Dimension 1")),
          yaxis = list(title = paste(dataset1$result$method, "Dimension 2")),
          showlegend = TRUE,
          legend = list(title = list(text = dataset1$result$color_col))
        )
    }
    
    return(p)
  })

  output$sidebyside_plot2 <- plotly::renderPlotly({
    dataset2 <- sidebyside_dataset2()
    if (is.null(dataset2)) {
      return(plotly::plot_ly() %>% 
        plotly::add_text(x = 0.5, y = 0.5, text = "Select datasets to compare", 
                        textfont = list(size = 16, color = "gray")) %>%
        plotly::layout(xaxis = list(visible = FALSE), yaxis = list(visible = FALSE)))
    }
    
    color_pal <- dataset2$color_palette
    if (is.null(color_pal)) {
      color_as_factor <- as.factor(dataset2$result$color_values)
      color_pal <- scales::hue_pal()(length(levels(color_as_factor)))
      names(color_pal) <- levels(color_as_factor)
    }

    if (input$enable_linked_brushing && !is.null(sidebyside_shared_data2())) {
      plot_data <- sidebyside_shared_data2()

      p <- plot_data %>%
        plotly::plot_ly(x = ~x, y = ~y, color = ~color,
                       colors = color_pal,
                       type = "scatter", mode = "markers",
                       marker = list(size = 6, opacity = 0.7),
                       hovertemplate = "<b>%{color}</b><br>X: %{x:.3f}<br>Y: %{y:.3f}<extra></extra>",
                       source = "sidebyside_plot2") %>%
        plotly::layout(
          title = list(text = dataset2$name, font = list(size = 14)),
          xaxis = list(title = paste(dataset2$result$method, "Dimension 1")),
          yaxis = list(title = paste(dataset2$result$method, "Dimension 2")),
          showlegend = TRUE,
          legend = list(title = list(text = dataset2$result$color_col)),
          dragmode = "select"
        ) %>%
        plotly::highlight(
          on = "plotly_selected", 
          off = "plotly_deselect",
          opacityDim = 0.3,
          selected = plotly::attrs_selected(opacity = 1)
        )
    } else {
      p <- plotly::plot_ly(x = dataset2$result$coords[, 1], 
                          y = dataset2$result$coords[, 2],
                          color = dataset2$result$color_values,
                          colors = color_pal,
                          type = "scatter", mode = "markers",
                          marker = list(size = 6, opacity = 0.7),
                          hovertemplate = "<b>%{color}</b><br>X: %{x:.3f}<br>Y: %{y:.3f}<extra></extra>") %>%
        plotly::layout(
          title = list(text = dataset2$name, font = list(size = 14)),
          xaxis = list(title = paste(dataset2$result$method, "Dimension 1")),
          yaxis = list(title = paste(dataset2$result$method, "Dimension 2")),
          showlegend = TRUE,
          legend = list(title = list(text = dataset2$result$color_col))
        )
    }
    
    return(p)
  })
  
  output$linked_brushing_status <- shiny::renderText({
    if (!isTRUE(input$enable_linked_brushing)) {
      return("Linked brushing is disabled")
    }
    
    if (is.null(sidebyside_shared_data1()) || is.null(sidebyside_shared_data2())) {
      return("Ready for comparison")
    }
    
    return("Linked brushing is active")
  })
  
  shiny::observeEvent(input$sidebyside_plot1_selected, {
    if (input$enable_linked_brushing) {
      cat("Selection event from plot 1:", length(input$sidebyside_plot1_selected$pointNumber), "points\n")
    }
  })
  
  shiny::observeEvent(input$sidebyside_plot2_selected, {
    if (input$enable_linked_brushing) {
      cat("Selection event from plot 2:", length(input$sidebyside_plot2_selected$pointNumber), "points\n")
    }
  })

  shiny::observeEvent(input$enable_brushing, {
    if (!isTRUE(input$enable_brushing) && !is.null(shared_vis_data())) {
      current_data <- shared_vis_data()$data()
      new_shared_data <- crosstalk::SharedData$new(current_data, key = ~ row.names(current_data))
      shared_vis_data(new_shared_data)
      shiny::showNotification("Selection cleared", type = "message", duration = 1)
    }
  })
}
