context("test-sp-anglr.R")
library(rgl)
library(maptools)
data(wrld_simpl)
library(raster)


test_that("sp works", {
  for (i in sample(seq_len(nrow(wrld_simpl)), 10)) {
    expect_silent(cmesh <- anglr(wrld_simpl[i, ]))
    
  }
  
})
