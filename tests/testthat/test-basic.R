library(Polaris)

test_that("core package functions exist and are callable", {
  expect_true(exists("run_nldr_viz"))
  expect_true(exists("nldr_viz_ui"))
  expect_true(exists("nldr_viz_server"))
  expect_true(exists("load_custom_datasets"))
  
  expect_true(is.function(run_nldr_viz))
  expect_true(is.function(nldr_viz_ui))
  expect_true(is.function(nldr_viz_server))
  expect_true(is.function(load_custom_datasets))
})

test_that("load_custom_datasets works correctly", {
  datasets <- load_custom_datasets()
  
  expect_type(datasets, "list")
  expect_gt(length(datasets), 0)
  
  expect_true("four_clusters" %in% names(datasets))
  expect_true("pdfsense" %in% names(datasets))
  expect_true("trees" %in% names(datasets))
  
  expect_s3_class(datasets$four_clusters, "data.frame")
  expect_s3_class(datasets$pdfsense, "data.frame")
  expect_s3_class(datasets$trees, "data.frame")
})

test_that("app creation works without errors", {
  expect_no_error({
    app <- run_nldr_viz()
  })
  app <- run_nldr_viz()
  expect_s3_class(app, "shiny.appobj")
})

test_that("UI creation works without errors", {
  expect_no_error({
    ui <- nldr_viz_ui()
  })
  ui <- nldr_viz_ui()
  expect_true("shiny.tag.list" %in% class(ui))
})

test_that("server function creation works without errors", {
  expect_no_error({
    server <- nldr_viz_server
  })
  expect_true(is.function(nldr_viz_server))
})

test_that("package namespace is properly configured", {
  expect_true("Polaris" %in% loadedNamespaces())
  
  exported_functions <- getNamespaceExports("Polaris")
  expect_true("run_nldr_viz" %in% exported_functions)
  expect_true("nldr_viz_ui" %in% exported_functions)
  expect_true("nldr_viz_server" %in% exported_functions)
  expect_true("load_custom_datasets" %in% exported_functions)
})
