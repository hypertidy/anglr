context("test-sp-anglr.R")
library(rgl)
library(maptools)
data(wrld_simpl)
library(raster)


test_that("sp works", {
  for (i in seq_len(nrow(wrld_simpl))) {
  cmesh <- anglr(wrld_simpl[i, ])
  }
  
})
