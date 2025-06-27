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

    bslib::nav_panel(
      title = "Dataset Preview",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = 300,
          gap = "3rem",
          bslib::card(title = "Data Options",
                      shiny::fileInput("file", "Upload Dataset", accept = c(".csv")),
                      shiny::selectInput("example_data", "Example Datasets",
                                         choices = c("None", "four_clusters", "pdfsense", "trees"))),
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
          )),

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

            # t-SNE parameters
            shiny::conditionalPanel(
              condition = "input.nldr_method == 't-SNE'",
              shiny::sliderInput("perplexity", "Perplexity:",
                                 min = 5, max = 50, value = 30, step = 1),
              shiny::sliderInput("max_iter_tsne", "Max Iterations:",
                                 min = 100, max = 2000, value = 1000, step = 100),
              shiny::checkboxInput("auto_perplexity", "Auto-adjust perplexity", value = FALSE)
            ),

            # UMAP parameters
            shiny::conditionalPanel(
              condition = "input.nldr_method == 'UMAP'",
              shiny::sliderInput("n_neighbors", "Number of Neighbors:",
                                 min = 2, max = 50, value = 15, step = 1),
              shiny::sliderInput("min_dist", "Min. Distance:",
                                 min = 0.01, max = 0.99, value = 0.1, step = 0.01)
            )),

          bslib::card(
            bslib::card_header("Color Options"),
            shiny::checkboxInput("auto_color", "Auto-select color column", value = TRUE),
            shiny::conditionalPanel(
              condition = "!input.auto_color",
              shiny::uiOutput("color_column_selection")
            )),

          bslib::card(
            bslib::card_header("Reproducibility Options"),
            shiny::numericInput("seed", "Random Seed:", value = 123, min = 1, max = 99999),
            shiny::actionButton("reset_seed", "Generate New Seed", class = "btn-info btn-sm"),
            shiny::downloadButton("download_settings", "Export Settings", class = "btn-primary btn-sm mt-2")
          ),

          shiny::actionButton("run_visualization", "Run Visualization", class = "btn-success")
        ),

        bslib::card(
          bslib::card_header("Visualization"),
          plotly::plotlyOutput("nldr_plot", height = "600px", width = "600px")
        ),

        bslib::card(
          bslib::card_header("Visualization Information"),
          shiny::verbatimTextOutput("vis_info")
        )
      )
    ),

    bslib::nav_spacer(),

    bslib::nav_panel(
      title = "Diagnoising",
      bslib::card(
        bslib::card_header("About NLDR Visualization Tool"),
        bslib::card_body(
          shiny::h3("Non-Linear Dimensionality Reduction Visualization Tool"),
          shiny::p("This application allows you to visualize high-dimensional data using two popular non-linear dimensionality reduction techniques:"),
          shiny::tags$ul(
            shiny::tags$li(shiny::strong("t-SNE (t-Distributed Stochastic Neighbor Embedding):"),
                           " A technique for dimensionality reduction that is particularly well suited for the visualization of high-dimensional datasets."),
            shiny::tags$li(shiny::strong("UMAP (Uniform Manifold Approximation and Projection):"),
                           " A dimensionality reduction technique that can be used for visualization similar to t-SNE, but also for general non-linear dimension reduction.")
          ),
          shiny::p("Use the Dataset Preview tab to load and inspect your data, then move to the Dataset Visualization tab to create interactive visualizations.")
        )
      )
    ),

    bslib::nav_spacer(),
  )
}
