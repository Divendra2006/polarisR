### This script is to generate an example data structure

## To import necessary packages
# devtools::install_github("JayaniLakshika/cardinalR")
library(cardinalR)

## To generate cluster locations
locations_mat <- gen_clustloc(p = 4, k = 4)

## To generate data
four_clusters <- gen_multicluster(n = c(300, 250, 650, 800), p = 4, k = 4,
                       loc = locations_mat,
                       scale = c(0.3, 0.7, 1, 1),
                       shape = c("gaussian", "pyrrect", "crescent", "quadratic"),
                       rotation = NULL,
                       is_bkg = FALSE)
## To save data
usethis::use_data(four_clusters, overwrite = TRUE)


