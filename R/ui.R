#' NLDR Visualization Tool UI
#'
#' Creates the user interface for the NLDR visualization tool
#' @import shiny
#' @importFrom bslib page_sidebar card card_header card_body layout_sidebar nav_panel nav_spacer
#' @return A Shiny UI function
#' @keywords internal
nldr_viz_ui <- function() {
  bslib::page_navbar(
    title = "NLDR Visualization Tool",
    theme = bslib::bs_theme(bootswatch = "lumen"),

    bslib::nav_spacer(),

    # Dataset Preview Tab
    bslib::nav_panel(
      title = "Dataset Preview",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = 300,
          gap = "3rem",
          bslib::card(
            title = "Data Options",
            shiny::fileInput("file", "Upload Dataset", accept = c(".csv")),
            shiny::selectInput("example_data", "Example Datasets",
                               choices = c("None", "four_clusters", "pdfsense", "trees"))
          ),
          bslib::card(
            bslib::card_header("Column Selection"),
            shiny::checkboxInput("auto_select", "Auto-select columns", value = TRUE),
            shiny::conditionalPanel(
              condition = "!input.auto_select",
              shiny::uiOutput("column_selection"),
              shiny::actionButton("reset_columns", "Reset Column Selection", class = "btn-warning")
            ),
            shiny::hr(),
            shiny::actionButton("apply_changes", "Apply Changes", class = "btn-primary")
          ),
          bslib::card(
            bslib::card_header("NLDR Datasets"),
            shiny::uiOutput("stored_nldr_ui")
          )
        ),

        bslib::card(
          bslib::card_header("Dataset Preview"),
          DT::DTOutput("data_preview")
        ),

        bslib::card(
          bslib::card_header("Dataset Information"),
          shiny::tableOutput("data_info")
        )
      )
    ),

    bslib::nav_spacer(),

    # Dataset Visualization Tab
    bslib::nav_panel(
      title = "Dataset Visualization",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = 300,
          gap = "3rem",
          bslib::card(
            bslib::card_header("Visualization Options"),
            shiny::radioButtons("nldr_method", "Choose Method:",
                                choices = c("t-SNE", "UMAP"),
                                selected = "t-SNE"),

            shiny::conditionalPanel(
              condition = "input.nldr_method == 't-SNE'",
              shiny::sliderInput("perplexity", "Perplexity:",
                                 min = 5, max = 50, value = 30, step = 1),
              shiny::sliderInput("max_iter_tsne", "Max Iterations:",
                                 min = 100, max = 2000, value = 1000, step = 100),
              shiny::checkboxInput("auto_perplexity", "Auto-adjust perplexity", value = FALSE)
            ),

            shiny::conditionalPanel(
              condition = "input.nldr_method == 'UMAP'",
              shiny::sliderInput("n_neighbors", "Number of Neighbors:",
                                 min = 2, max = 50, value = 15, step = 1),
              shiny::sliderInput("min_dist", "Min. Distance:",
                                 min = 0.01, max = 0.99, value = 0.1, step = 0.01)
            )
          ),

          bslib::card(
            bslib::card_header("Color Options"),
            shiny::checkboxInput("auto_color", "Auto-select color column", value = TRUE),
            shiny::conditionalPanel(
              condition = "!input.auto_color",
              shiny::uiOutput("color_column_selection")
            )
          ),

          bslib::card(
            bslib::card_header("Reproducibility Options"),
            shiny::numericInput("seed", "Random Seed:", value = 123, min = 1, max = 99999)
          ),

          shiny::actionButton("run_visualization", "Run Visualization", class = "btn-success")
        ),

        bslib::layout_columns(
          col_widths = c(8, 4),
          bslib::card(
            bslib::card_header("Visualization"),
            plotly::plotlyOutput("nldr_plot", height = "800px", width = "100%")
          ),

          bslib::card(
            bslib::card_header("Visualization Information"),
            shiny::verbatimTextOutput("vis_info")
          )
        )
      )
    ),

    bslib::nav_spacer(),

    # Dynamic Tour Tab
    bslib::nav_panel(
      title = "Dynamic Tour",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = 300,
          bslib::card(
            bslib::card_header("Tour Options"),

            shiny::radioButtons("tour_display_type", "Select Tour Display:",
                                choices = c("Scatter", "Sage", "Slice"), inline = TRUE),
            shiny::hr(),
            shiny::checkboxInput("tour_axes", "Show Axes", value = FALSE),
            shiny::hr(),
            shiny::conditionalPanel(
              condition = "input.tour_display_type == 'Scatter'",
              shiny::sliderInput("tour_alpha", "Point Opacity (Alpha):",
                                 min = 0.1, max = 1, value = 0.7, step = 0.05)
            ),
            shiny::conditionalPanel(
              condition = "input.tour_display_type == 'Sage'",
              shiny::sliderInput("tour_gamma", "Gamma:",
                                 min = 0, max = 5, value = 1, step = 0.1)
            ),
            shiny::conditionalPanel(
              condition = "input.tour_display_type == 'Slice'",
              shiny::sliderInput("tour_slice_volume", "Slice Relative Volume:",
                                 min = 0.01, max = 0.5, value = 0.1, step = 0.01)
            ),
            shiny::hr(),
            shiny::checkboxInput("enable_brushing", "Enable Linked Brushing", value = TRUE)
          )
        ),

        bslib::layout_columns(
          col_widths = c(6, 6),
          height = "700px",
          bslib::card(
            bslib::card_header("NLDR Visualization"),
            bslib::card_body(
              style = "padding-top: 10px; padding-bottom: 10px;",
              plotly::plotlyOutput("nldr_plot_tour_tab", height = "580px")
            )
          ),
          bslib::card(
            bslib::card_header("Dynamic Tour of High-Dimensional Data"),
            bslib::card_body(
              style = "padding-top: 10px; padding-bottom: 10px;",
              shiny::uiOutput("dynamic_tour_output_ui")
            )
          )
        )
      )
    ),

    bslib::nav_spacer(),

    bslib::nav_panel(
      title = "Diagnosing",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = 300,
          gap = "3rem",

          # Binwidth Optimization Section
          bslib::card(
            bslib::card_header("Binwidth Optimization"),
            shiny::numericInput("min_bins", "Minimum Bins:",
                                value = 5, min = 3, max = 20, step = 1),
            shiny::numericInput("max_bins", "Maximum Bins:",
                                value = 15, min = 5, max = 50, step = 1),
            shiny::checkboxInput("auto_bin_range", "Auto-calculate range", value = TRUE),
            shiny::hr(),
            shiny::actionButton("run_binwidth_optimization", "Optimize Binwidth",
                                class = "btn-info", style = "width: 100%;")
          ),

          # NEW: Low-Density Hexagon Removal Card
          bslib::card(
            bslib::card_header("Low-Density Hexagon Removal"),
            shiny::checkboxInput("quollr_remove_low_density", "Remove low-density hexagons", value = FALSE),
            shiny::conditionalPanel(
              condition = "input.quollr_remove_low_density",
              shiny::numericInput("quollr_density_threshold", "Density Threshold:",
                                  value = 0.1, min = 0.01, max = 1, step = 0.01),
              shiny::helpText("Remove hexagons with density below this threshold")
            )
          ),

          # Analysis Actions (simplified - no manual configuration needed)
          bslib::card(
            bslib::card_header("Analysis Actions"),
            shiny::div(
              style = "margin-bottom: 10px;",
              shiny::helpText("Note: Run binwidth optimization first to automatically configure optimal bins")
            ),
            shiny::actionButton("run_quollr_analysis", "Run Quollr Analysis",
                                class = "btn-success"),
            shiny::actionButton("show_langevitour", "Show 3D Tour",
                                class = "btn-secondary", style = "width: 100%;")
          )
        ),

        # Main content area with Analysis on the left and Summary on the right
        bslib::layout_columns(
          col_widths = c(8, 4),

          # Column 1: Analysis Results Card
          bslib::card(
            bslib::card_header("Analysis Results"),
            shiny::tabsetPanel(
              shiny::tabPanel("MSE vs Binwidth",
                              plotly::plotlyOutput("binwidth_mse_plot", height = "500px")),
              shiny::tabPanel("Optimization Table",
                              DT::DTOutput("binwidth_results_table")),
              shiny::tabPanel("Model Fit",
                              plotly::plotlyOutput("quollr_fit_plot", height = "500px")),
              shiny::tabPanel("High-Dimensional Model Tour",
                              shiny::conditionalPanel(
                                condition = "output.show_langevitour_ui",
                                shiny::uiOutput("langevitour_output", height = "500px")
                              ))
            )
          ),

          # Column 2: Enhanced Summary Card
          bslib::card(
            bslib::card_header("Configuration & Summary"),
            bslib::card_body(
              # Current Configuration Section
              shiny::h6("Current Configuration"),
              shiny::div(
                id = "current_config_display",
                style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; margin-bottom: 15px;",
                shiny::verbatimTextOutput("current_config_summary")
              ),

              # Optimization Results Section
              shiny::h6("Optimization Results"),
              shiny::verbatimTextOutput("optimal_binwidth_summary"),
              shiny::hr(),

              # Model Evaluation Section
              shiny::h6("Model Evaluation"),
              shiny::verbatimTextOutput("quollr_model_summary")
            )
          )
        )
      )
    ),
    bslib::nav_spacer()
  )
}
