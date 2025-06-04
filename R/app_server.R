#' Application Server
#' @import shiny
#' @importFrom dplyr select_if
#' @importFrom tools file_path_sans_ext
#' @export

app_server <- function(input, output, session) {

  source("R/data_loader.R", local = TRUE)$value
  source("R/plot_utils.R", local = TRUE)$value

  # Navigation logic
  observeEvent(input$nav_visualization, {
    updateNavbarPage(session, "main_navbar", selected = "visualization_panel")
  })

  observeEvent(input$nav_diagnosing, {
    updateNavbarPage(session, "main_navbar", selected = "diagnosing_panel")
  })

  uploaded_file <- reactiveVal(NULL)

  observeEvent(input$file, {
    if (!is.null(input$file)) {
      uploaded_file(input$file)
      updateSelectInput(session, "dataset", selected = "none")
    }
  })

  output$remove_dataset_ui <- renderUI({
    if (!is.null(uploaded_file())) {
      tagList(
        textInput("dataset_name", "Dataset Name",
                  value = tools::file_path_sans_ext(uploaded_file()$name)),
        actionButton("remove_file", "Remove Dataset", class = "btn-danger")
      )
    }
  })

  observeEvent(input$remove_file, uploaded_file(NULL))

  # Auto-adjust perplexity
  observe({
    if (input$auto_perplexity && !is.null(dataset())) {
      n_rows <- nrow(dataset()$data)
      suggested_perplexity <- min(max(round(sqrt(n_rows)/3), 5), 50)
      updateSliderInput(session, "perplexity", value = suggested_perplexity)
    }
  })

  dataset <- reactive(load_dataset(input, uploaded_file))
  output$color_selection <- renderUI(render_color_ui(dataset()))
  output$preview <- DT::renderDT(render_preview(dataset()))
  results <- eventReactive(input$run, run_analysis(input, dataset()))
  output$plot <- plotly::renderPlotly(render_plot(results()))
  output$diagnosticInfo <- renderText(results()$diagnostic)
  output$executionTime <- renderText(
    paste("Execution time:", round(results()$execution_time, 2), "seconds")
  )
}
