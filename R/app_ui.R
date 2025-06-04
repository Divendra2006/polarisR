#' Application UI
#' @import shiny
#' @import bslib
#' @export

app_ui <- function() {
  page_navbar(
    title = "NLDR Diagnostic Tool",
    id = "main_navbar",
    # Add custom styling
    tags$head(
      tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Poppins:wght@300;400;500;600;700&display=swap"),
      tags$style(HTML("
        :root {
          --primary: #4361ee;
          --primary-dark: #3a56d4;
          --secondary: #3f37c9;
          --light: #f8f9fa;
          --dark: #212529;
          --gray: #6c757d;
          --light-blue: #e6f0ff;
        }

        body {
          font-family: 'Poppins', sans-serif;
          margin: 0;
          padding: 0;
          color: var(--dark);
          line-height: 1.6;
        }

        /* Landing Page Styles */
        .landing-page {
          background: linear-gradient(135deg, var(--light-blue) 0%, #ffffff 100%);
          height: 100vh;
          display: flex;
          justify-content: center;
          align-items: center;
          flex-direction: column;
          text-align: center;
          padding: 2rem;
        }

        .landing-page h1 {
          font-size: 2.5rem;
          font-weight: 600;
          color: var(--dark);
          margin-bottom: 1.5rem;
          line-height: 1.3;
        }

        .landing-page p {
          font-size: 1.1rem;
          color: var(--gray);
          max-width: 600px;
          margin-bottom: 2.5rem;
        }

        .button-container {
          display: flex;
          gap: 20px;
          justify-content: center;
          flex-wrap: wrap;
        }

        .start-button {
          background-color: var(--primary);
          color: white;
          border: none;
          padding: 0.8rem 2rem;
          font-size: 1.1rem;
          font-weight: 500;
          border-radius: 50px;
          cursor: pointer;
          transition: all 0.3s ease;
          box-shadow: 0 4px 15px rgba(67, 97, 238, 0.3);
        }

        .start-button:hover {
          background-color: var(--primary-dark);
          transform: translateY(-2px);
          box-shadow: 0 6px 20px rgba(67, 97, 238, 0.4);
        }
      "))
    ),

    nav_panel(
      "Home",
      div(
        class = "landing-page",
        h1("Interactive Diagnostics Tools for NLDR Visualization"),
        p("Explore and analyze your data with powerful dimensionality reduction techniques"),
        div(
          class = "button-container",
          actionButton("nav_visualization", "NLDR Method Visualization", class = "start-button"),
          actionButton("nav_diagnosing", "NLDR Method Diagnosing", class = "start-button")
        )
      )
    ),
    nav_panel_hidden("visualization_panel", visualization_ui()),
    nav_panel_hidden("diagnosing_panel", h3("Diagnosing Content Here"))
  )
}

#' Visualization Module UI
#' @importFrom plotly plotlyOutput
visualization_ui <- function() {
  layout_sidebar(
    sidebar = sidebar(
      fileInput("file", "Upload Dataset (CSV file)", accept = ".csv"),
      uiOutput("remove_dataset_ui"),
      conditionalPanel(
        condition = "input.file == null",
        selectInput("dataset", "Sample Dataset",
                    choices = c("Four Clusters" = "four_clusters",
                                "PDFSense" = "pdfsense",
                                "Trees" = "trees"))
      ),
      radioButtons("method", "Dimensionality Reduction Method",
                   choices = c("t-SNE", "UMAP"),
                   selected = "t-SNE"),
      conditionalPanel(
        condition = "input.method == 't-SNE'",
        sliderInput("perplexity", "Perplexity", min = 5, max = 50, value = 30),
        checkboxInput("auto_perplexity", "Auto-adjust perplexity", value = TRUE),
        sliderInput("tsne_iterations", "Max Iterations", min = 100, max = 2000, value = 1000)
      ),
      conditionalPanel(
        condition = "input.method == 'UMAP'",
        sliderInput("n_neighbors", "Number of Neighbors", min = 5, max = 50, value = 15),
        sliderInput("min_dist", "Minimum Distance", min = 0.01, max = 0.99, value = 0.1)
      ),
      uiOutput("color_selection"),
      actionButton("run", "Run Analysis", class = "btn-primary")
    ),
    navset_card_tab(
      nav_panel("Visualization", plotlyOutput("plot", height = "500px")),
      nav_panel("Dataset Preview", DT::DTOutput("preview")),
      nav_panel("Diagnostic Information",
                textOutput("diagnosticInfo"),
                verbatimTextOutput("executionTime"))
    )
  )
}
