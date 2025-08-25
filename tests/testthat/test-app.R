library(polarisR)

test_that("run_nldr_viz creates a Shiny app", {
  expect_true(exists("run_nldr_viz"))
  expect_true(is.function(run_nldr_viz))
  
  expect_no_error({
    app <- run_nldr_viz()
  })
  app <- run_nldr_viz()
  expect_s3_class(app, "shiny.appobj")
  
  expect_true(!is.null(app$httpHandler))
  expect_true(!is.null(app$serverFuncSource))
})

test_that("run_nldr_viz function signature is correct", {
  app <- run_nldr_viz()
  expect_s3_class(app, "shiny.appobj")
  
  formals_list <- formals(run_nldr_viz)
  expect_equal(length(formals_list), 1)
  expect_equal(names(formals_list), "...")
})

test_that("nldr_viz_ui creates valid UI", {
  expect_true(exists("nldr_viz_ui"))
  expect_true(is.function(nldr_viz_ui))
  
  expect_no_error({
    ui <- nldr_viz_ui()
  })
  
  ui <- nldr_viz_ui()
  expect_true("shiny.tag.list" %in% class(ui))
  
  ui_html <- as.character(ui)
  expect_true(nchar(ui_html) > 100)
  
  expect_true(grepl("Dataset Preview", ui_html))
  expect_true(grepl("Non-linear dimension reduction \\(NLDR\\)", ui_html))
  expect_true(grepl("Dynamic Tour", ui_html))
  expect_true(grepl("Diagnosing", ui_html))
  expect_true(grepl("2-D Layout Comparison", ui_html))
})

test_that("UI contains required input elements", {
  ui <- nldr_viz_ui()
  ui_html <- as.character(ui)
  
  expect_true(grepl('id="file"', ui_html))
  expect_true(grepl('id="example_data"', ui_html))
  expect_true(grepl('id="nldr_method"', ui_html))
  expect_true(grepl('id="perplexity"', ui_html))
  expect_true(grepl('id="run_visualization"', ui_html))
  expect_true(grepl('id="auto_select"', ui_html))
  expect_true(grepl('id="apply_changes"', ui_html))
})

test_that("UI contains required output elements", {
  ui <- nldr_viz_ui()
  ui_html <- as.character(ui)
  
  expect_true(grepl('id="data_preview"', ui_html))
  expect_true(grepl('id="data_info"', ui_html))
  expect_true(grepl('id="nldr_plot"', ui_html))
  expect_true(grepl('id="vis_info"', ui_html))
})

test_that("nldr_viz_server is a valid server function", {
  expect_true(exists("nldr_viz_server"))
  expect_true(is.function(nldr_viz_server))
  
  server_formals <- formals(nldr_viz_server)
  expect_true("input" %in% names(server_formals))
  expect_true("output" %in% names(server_formals))
  expect_true("session" %in% names(server_formals))
})

test_that("app UI and server are properly integrated", {
  app <- run_nldr_viz()
  
  expect_true(!is.null(app$httpHandler))
  expect_true(!is.null(app$serverFuncSource))
  
  expect_true(is.function(app$serverFuncSource()))
})

test_that("package namespace is properly configured", {
  expect_true("polarisR" %in% loadedNamespaces())
  
  exported_functions <- getNamespaceExports("polarisR")
  expect_true("run_nldr_viz" %in% exported_functions)
  expect_true("nldr_viz_ui" %in% exported_functions)
  expect_true("nldr_viz_server" %in% exported_functions)
  expect_true("load_custom_datasets" %in% exported_functions)
})
