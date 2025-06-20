# download PDFsense data - run this once to create the data file
# NOTE: This script should be run once to generate the pdfsense.rda file
# and then commented out or removed from the package

library(readr)
options(timeout = 300)

# Only run this data downloading code if the data doesn't already exist
if (!exists("pdfsense") && !file.exists("data/pdfsense.rda")) {
  message("Downloading PDFsense data...")

  url <- "http://www.physics.smu.edu/botingw/PDFsense_web_histlogy/tsv.zip"
  dir <- tempdir()
  zip <- file.path(dir, "pdfsense.zip")

  tsv_path <- file.path(dir, "tsv", "CT14_signed")
  download.file(url, destfile = zip)

  unzip(zip, exdir = dir)

  mat <- read_tsv(
    file.path(tsv_path, "residual_all_norm_-1_RawData.tsv"),
    col_names = FALSE
  )

  meta <- read_tsv(
    file.path(tsv_path, "metadata_RawData.tsv")
  )

  pdfsense <- as.data.frame(cbind(meta, mat))

  unlink(dir, recursive = TRUE)

  # Save the data to the package's data directory
  if (!dir.exists("data")) {
    dir.create("data")
  }

  # Add documentation for the dataset
  usethis::use_data(pdfsense, overwrite = TRUE)
  message("PDFsense data saved to data/pdfsense.rda")
} else {
  message("PDFsense data already exists, skipping download")
}
