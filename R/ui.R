#' NLDR Visualization Tool UI
#'
#' Creates the user interface for the NLDR visualization tool
#' @import shiny
#' @importFrom bslib page_navbar bs_theme nav_panel layout_sidebar sidebar card card_header card_body page_sidebar
#' @importFrom DT DTOutput
#' @importFrom plotly plotlyOutput
#' @return A Shiny UI function
#' @export
nldr_viz_ui <- function() {
  bslib::page_navbar(
    title = "NLDR Visualization Tool",
    theme = bslib::bs_theme(bootswatch = "lumen"),
    header = shiny::tags$head(
      shiny::tags$style(shiny::HTML("
        .btn.disabled {
          pointer-events: none;
          opacity: 0.6;
        }
        .fa-spin {
          animation: fa-spin 2s infinite linear;
        }
        @keyframes fa-spin {
          0% { transform: rotate(0deg); }
          100% { transform: rotate(360deg); }
        }
      "))
    ),
    
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
              choices = c("None", "four_clusters", "pdfsense", "trees")
            )
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
              selected = "t-SNE"
            ),
            shiny::conditionalPanel(
              condition = "input.nldr_method == 't-SNE'",
              shiny::sliderInput("perplexity", "Perplexity:",
                min = 5, max = 50, value = 30, step = 1
              ),
              shiny::sliderInput("max_iter_tsne", "Max Iterations:",
                min = 100, max = 2000, value = 1000, step = 100
              ),
              shiny::checkboxInput("auto_perplexity", "Auto-adjust perplexity", value = FALSE)
            ),
            shiny::conditionalPanel(
              condition = "input.nldr_method == 'UMAP'",
              shiny::sliderInput("n_neighbors", "Number of Neighbors:",
                min = 2, max = 50, value = 15, step = 1
              ),
              shiny::sliderInput("min_dist", "Min. Distance:",
                min = 0.01, max = 0.99, value = 0.1, step = 0.01
              )
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
          shiny::div(
            shiny::conditionalPanel(
              condition = "!output.visualization_button_disabled",
              shiny::actionButton("run_visualization", "Run Visualization", class = "btn-success w-100")
            ),
            shiny::conditionalPanel(
              condition = "output.visualization_button_disabled",
              shiny::div(
                class = "btn btn-success disabled w-100",
                shiny::icon("spinner", class = "fa-spin"),
                " Processing..."
              )
            )
          )
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
    
    bslib::nav_panel(
      title = "Dynamic Tour",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = 300,
          bslib::card(
            bslib::card_header("Tour Options"),
            shiny::radioButtons("tour_display_type", "Select Tour Display:",
              choices = c("Scatter", "Sage", "Slice"), inline = TRUE
            ),
            shiny::hr(),
            shiny::checkboxInput("tour_axes", "Show Axes", value = FALSE),
            shiny::checkboxInput("show_edges", "Show Edges (5-NN Graph)", value = FALSE),
            shiny::hr(),
            shiny::conditionalPanel(
              condition = "input.tour_display_type == 'Scatter'",
              shiny::sliderInput("tour_alpha", "Point Opacity (Alpha):",
                min = 0.1, max = 1, value = 0.7, step = 0.05
              )
            ),
            shiny::conditionalPanel(
              condition = "input.tour_display_type == 'Sage'",
              shiny::sliderInput("tour_gamma", "Gamma:",
                min = 0, max = 5, value = 1, step = 0.1
              )
            ),
            shiny::conditionalPanel(
              condition = "input.tour_display_type == 'Slice'",
              shiny::sliderInput("tour_slice_volume", "Slice Relative Volume:",
                min = 0.01, max = 0.5, value = 0.1, step = 0.01
              )
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
              padding = "0.5rem",
              plotly::plotlyOutput("nldr_plot_tour_tab", height = "600px")
            )
          ),
          bslib::card(
            bslib::card_header("Dynamic Tour of High-Dimensional Data"),
            bslib::card_body(
              padding = "0.5rem",
              shiny::uiOutput("dynamic_tour_output_ui")
            )
          )
        )
      )
    ),
    
    bslib::nav_panel(
      title = "Diagnosing",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = 300,
          gap = "3rem",
          bslib::card(
            bslib::card_header("Binwidth Optimization"),
            shiny::helpText("Click the button to automatically test a range of bin widths and find the optimal configuration based on RMSE."),
            shiny::hr(),
            shiny::div(
              shiny::conditionalPanel(
                condition = "!output.binwidth_button_disabled",
                shiny::actionButton("run_binwidth_optimization", "Optimize Binwidth", class = "btn-info w-100")
              ),
              shiny::conditionalPanel(
                condition = "output.binwidth_button_disabled",
                shiny::div(
                  class = "btn btn-info disabled w-100",
                  shiny::icon("spinner", class = "fa-spin"),
                  " Optimizing..."
                )
              )
            )
          ),
          bslib::card(
            bslib::card_header("Analysis Actions"),
            shiny::helpText("Note: Run binwidth optimization first to automatically configure optimal bins", class = "mb-3"),
            shiny::div(
              class = "d-grid gap-2",
              shiny::div(
                shiny::conditionalPanel(
                  condition = "!output.quollr_button_disabled",
                  shiny::actionButton("run_quollr_analysis", "Run Quollr Analysis", class = "btn-success w-100")
                ),
                shiny::conditionalPanel(
                  condition = "output.quollr_button_disabled",
                  shiny::div(
                    class = "btn btn-success disabled w-100",
                    shiny::icon("spinner", class = "fa-spin"),
                    " Analyzing..."
                  )
                )
              ),
              shiny::actionButton("show_langevitour", "Show 3D Tour", class = "btn-secondary w-100")
            )
          )
        ),
        bslib::layout_columns(
          col_widths = c(8, 4),
          gap = "20px",
          bslib::card(
            bslib::card_header("Analysis Results"),
            bslib::card_body(
              shiny::div(
                style = "padding-top: 30px;",
                shiny::tabsetPanel(
                  shiny::tabPanel(
                    "RMSE vs Binwidth",
                    shiny::div(
                      class = "my-5",
                      plotly::plotlyOutput("binwidth_mse_plot", height = "500px")
                    )
                  ),
                  shiny::tabPanel(
                    "Optimization Table",
                    shiny::div(
                      class = "my-5",
                      DT::DTOutput("binwidth_results_table")
                    )
                  ),
                  shiny::tabPanel(
                    "Model Fit",
                    shiny::div(
                      class = "my-5",
                      plotly::plotlyOutput("quollr_fit_plot", height = "500px")
                    )
                  ),
                  shiny::tabPanel(
                    "High-Dimensional Model Tour",
                    shiny::div(
                      style = "margin-top: 1rem;",
                      shiny::conditionalPanel(
                        condition = "output.show_langevitour_ui",
                        shiny::uiOutput("langevitour_output")
                      ),
                      shiny::conditionalPanel(
                        condition = "!output.show_langevitour_ui",
                        shiny::div(
                          class = "text-center p-5",
                          style = "border: 2px dashed #ccc; border-radius: 8px; color: #666; background-color: #f9f9f9; height: 500px; display: flex; flex-direction: column; justify-content: center; align-items: center;",
                          shiny::icon("film", "fa-3x"),
                          shiny::h5("3D Model Tour", class = "mt-3"),
                          shiny::p("Click 'Show 3D Tour' in the sidebar to generate the interactive model tour.")
                        )
                      )
                    )
                  )
                )
              )
            )
          ),
          bslib::card(
            bslib::card_header("Configuration & Summary"),
            bslib::card_body(
              style = "padding: 20px;",
              shiny::h6("Model Evaluation", style = "margin-bottom: 10px; color: #495057;"),
              shiny::verbatimTextOutput("optimal_binwidth_summary"),
              shiny::verbatimTextOutput("quollr_model_summary")
            )
          )
        )
      )
    ),
    
    bslib::nav_panel(
      title = "Method Comparison",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = 300,
          gap = "3rem",
          bslib::card(
            bslib::card_header("Comparison Type"),
            shiny::radioButtons(
              "comparison_type",
              tags$div(style = "margin-bottom: 10px;", "Choose Comparison Type:"),
              choices = c(
                "NLDR Settings Comparison" = "settings",
                "Side-by-Side Visualization" = "visualization"
              ),
              selected = "settings"
            )

          ),
          shiny::conditionalPanel(
            condition = "input.comparison_type == 'settings'",
            bslib::card(
              bslib::card_header("Dataset Selection"),
              shiny::helpText("To compare methods, create NLDRs, optimize binwidth, then select to compare.",
                class = "text-muted mb-3"
              ),
              shiny::uiOutput("comparison_dataset_selection"),
              shiny::hr(),
              shiny::div(
                shiny::conditionalPanel(
                  condition = "!output.comparison_button_disabled",
                  shiny::actionButton("run_comparison_analysis", "Run Comparison Plot",
                    class = "btn-primary w-100"
                  )
                ),
                shiny::conditionalPanel(
                  condition = "output.comparison_button_disabled",
                  shiny::div(
                    class = "btn btn-primary disabled w-100",
                    shiny::icon("spinner", class = "fa-spin"),
                    " Comparing..."
                  )
                )
              ),
              shiny::actionButton("clear_comparison_selection", "Clear Selection",
                class = "btn-warning w-100 mt-2"
              )
            )
          ),

          shiny::conditionalPanel(
            condition = "input.comparison_type == 'visualization'",
            bslib::card(
              bslib::card_header("Dataset Selection"),
              shiny::uiOutput("sidebyside_dataset1_selection"),
              shiny::uiOutput("sidebyside_dataset2_selection"),
              shiny::hr(),
              shiny::checkboxInput("enable_linked_brushing", "Enable Linked Brushing", value = TRUE),
              shiny::hr(),
              shiny::actionButton("generate_sidebyside_comparison", "Run Visualization",
                class = "btn-success w-100"
              ),
              shiny::actionButton("clear_sidebyside_selection", "Clear Selection",
                class = "btn-warning w-100 mt-2"
              )
            )
          )
        ),
        # Main content area with conditional panels
        shiny::conditionalPanel(
          condition = "input.comparison_type == 'settings'",
          bslib::layout_columns(
            col_widths = c(8, 4),
            gap = "20px",
            bslib::card(
              bslib::card_header("RMSE Comparison Plot"),
              bslib::card_body(
                plotly::plotlyOutput("comparison_mse_plot", height = "550px")
              )
            ),
            bslib::card(
              bslib::card_header("Best Configuration Summary"),
              bslib::card_body(
                style = "padding-top: 1.25rem;",
                shiny::verbatimTextOutput("best_configuration_summary")
              )
            )
          )
        ),
        shiny::conditionalPanel(
          condition = "input.comparison_type == 'visualization'",
          bslib::layout_columns(
            col_widths = c(6, 6),
            gap = "20px",
            height = "700px",
            bslib::card(
              bslib::card_header(shiny::textOutput("sidebyside_plot1_title")),
              bslib::card_body(
                padding = "0.5rem",
                plotly::plotlyOutput("sidebyside_plot1", height = "600px")
              )
            ),
            bslib::card(
              bslib::card_header(shiny::textOutput("sidebyside_plot2_title")),
              bslib::card_body(
                padding = "0.5rem",
                plotly::plotlyOutput("sidebyside_plot2", height = "600px")
              )
            )
          )
        )
      )
    )
  )
}
