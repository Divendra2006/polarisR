#' Load datasets
#' @keywords internal
load_dataset <- function(input, uploaded_file) {
  if (!is.null(uploaded_file())) {
    df <- read.csv(uploaded_file()$datapath)
    name <- ifelse(!is.null(input$dataset_name) && input$dataset_name != "",
                   input$dataset_name, "User Dataset")
    return(list(data = df, name = name))
  } else {
    data_name <- input$dataset
    if (data_name == "four_clusters") {
      return(list(data = nldrDiagnostics::four_clusters, name = "Four Clusters"))
    } else if (data_name == "pdfsense") {
      return(list(data = nldrDiagnostics::pdfsense, name = "PDFSense"))
    } else if (data_name == "trees") {
      return(list(data = nldrDiagnostics::fake_trees, name = "Trees"))
    }
  }
}

#' Render dataset preview
#' @keywords internal
render_preview <- function(dataset) {
  head(dataset$data, 10)
}

#' Render color selection UI
#' @keywords internal
render_color_ui <- function(dataset) {
  data <- dataset$data
  color_vars <- names(data)[sapply(names(data), function(col) {
    col_data <- data[[col]]
    is.character(col_data) || is.factor(col_data) ||
      (is.numeric(col_data) && length(unique(col_data)) <= 30) ||
      grepl("id|ID|Id", col, ignore.case = TRUE)
  })]

  default <- if ("cluster" %in% color_vars) "cluster" else color_vars[1]
  selectInput("color_var", "Color points by:",
              choices = c("None" = "none", color_vars),
              selected = default)
}
