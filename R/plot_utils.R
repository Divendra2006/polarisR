#' Run analysis
#' @importFrom Rtsne Rtsne
#' @importFrom umap umap
#' @keywords internal
run_analysis <- function(input, dataset) {
  start_time <- Sys.time()
  data <- dataset$data
  color_var <- if (!is.null(input$color_var) && input$color_var != "none") {
    data[[input$color_var]]
  } else {
    rep("1", nrow(data))
  }

  data_numeric <- data |> dplyr::select_if(is.numeric)

  if (input$method == "t-SNE") {
    set.seed(42)

    tsne_result <- Rtsne(data_numeric, dims = 2,
                         perplexity = input$perplexity,
                         max_iter = input$tsne_iterations, check_duplicates = FALSE, pca = TRUE)
    result_df <- data.frame(x = tsne_result$Y[,1], y = tsne_result$Y[,2], color = color_var)
    diagnostic <- paste("t-SNE: Perplexity =", input$perplexity,
                        "Iterations =", input$tsne_iterations)
  } else {
    set.seed(42)
    umap_result <- umap(data_numeric,
                        n_neighbors = input$n_neighbors,
                        min_dist = input$min_dist)
    result_df <- data.frame(x = umap_result$layout[,1], y = umap_result$layout[,2], color = color_var)
    diagnostic <- paste("UMAP: Neighbors =", input$n_neighbors,
                        "Min Distance =", input$min_dist)
  }

  list(
    result_df = result_df,
    diagnostic = diagnostic,
    execution_time = Sys.time() - start_time,
    dataset_name = dataset$name,
    color_var_name = ifelse(!is.null(input$color_var) && input$color_var != "none",
                            input$color_var, NULL)
  )
}

#' Render main plot
#' @import ggplot2
#' @importFrom plotly ggplotly
#' @keywords internal
render_plot <- function(results) {
  color_title <- results$color_var_name %||% "Group"
  p <- ggplot(results$result_df, aes(x = x, y = y, color = color)) +
    geom_point(alpha = 0.7) +
    labs(title = paste(input$method, "plot of", results$dataset_name),
         color = color_title) +
    theme_minimal()
  ggplotly(p)
}
